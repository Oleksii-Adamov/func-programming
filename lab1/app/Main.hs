import Database.HDBC
import Database.HDBC.ODBC

-- Define a class for insertable entities
class Insertable a where
    insert :: Connection -> a -> IO (Maybe Integer)

-- Define data types representing each table

data Student = Student {
    studentID :: Int,
    studentFirstName :: String,
    studentLastName :: String,
    studentEmail :: String,
    studentPassword :: String
}

data Teacher = Teacher {
    teacherID :: Int,
    teacherFirstName :: String,
    teacherLastName :: String,
    teacherEmail :: String,
    teacherPassword :: String
}

data Topic = Topic {
    topicID :: Int,
    topicTitle :: String,
    topicDescription :: String
}

data Schedule = Schedule {
    scheduleID :: Int,
    topicID :: Int,
    teacherID :: Int,
    startDate :: UTCTime,
    endDate :: UTCTime
} deriving (Show)

data Task = Task {
    taskID :: Int,
    studentID :: Int,
    topicID :: Int,
    taskDescription :: String,
    taskGrade :: Int
} deriving (Show)

data Material = Material {
    materialID :: Int,
    topicID :: Int,
    materialTitle :: String,
    materialLink :: String
} deriving (Show)

data Question = Question {
    questionID :: Int,
    studentID :: Int,
    questionText :: String
} deriving (Show)

data Answer = Answer {
    answerID :: Int,
    questionID :: Int,
    teacherID :: Int,
    answerText :: String
} deriving (Show)



-- Define instances for Insertable for each table
instance Insertable Student where
    insert conn student = do
      stmt <- prepare conn "INSERT INTO Students (FirstName, LastName, Email, Password) VALUES (?, ?, ?, ?)"
      execute stmt [toSql (studentFirstName student), toSql (studentLastName student), toSql (studentEmail student), toSql (studentPassword student)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT StudentID FROM Students WHERE ROWID = (SELECT MAX(ROWID) FROM Students)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

instance Insertable Teacher where
    insert conn teacher = do
      stmt <- prepare conn "INSERT INTO Teachers (FirstName, LastName, Email, Password) VALUES (?, ?, ?, ?)"
      execute stmt [toSql (teacherFirstName teacher), toSql (teacherLastName teacher), toSql (teacherEmail teacher), toSql (teacherPassword teacher)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT TeacherID FROM Teachers WHERE ROWID = (SELECT MAX(ROWID) FROM Teachers)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

instance Insertable Topic where
    insert conn topic = do
      stmt <- prepare conn "INSERT INTO TOPICS (TOPICTITLE, TOPICDESCRIPTION) VALUES (?, ?)"
      execute stmt [toSql (topicTitle topic), toSql (topicDescription topic)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT TOPICID FROM TOPICS WHERE ROWID = (SELECT MAX(ROWID) FROM TOPICS)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

insertSchedule :: Connection -> Int -> Int -> String -> String -> IO Integer
insertSchedule conn topicID teacherID startDateStr endDateStr = do
    let maybeStartDate = parseCustomDate startDateStr
        maybeEndDate = parseCustomDate endDateStr
    case (maybeStartDate, maybeEndDate) of
        (Just startDate, Just endDate) -> do
            stmt <- prepare conn "INSERT INTO Schedule (TopicID, TeacherID, StartDate, EndDate) VALUES (?, ?, ?, ?)"
            execute stmt [toSql topicID, toSql teacherID, toSql startDate, toSql endDate]
        _ -> putStrLn "Invalid date format"

instance Insertable Schedule where
    insert conn schedule = do
      stmt <- prepare conn "INSERT INTO Schedule (TopicID, TeacherID, StartDate, EndDate) VALUES (?, ?, ?, ?)"
      execute stmt [toSql (topicID schedule), toSql (teacherID schedule), toSql (startDate schedule), toSql (endDate schedule)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT ScheduleID FROM Schedule WHERE ROWID = (SELECT MAX(ROWID) FROM Schedule)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing


main :: IO ()
main = do
    conn <- connectODBC "DSN=colabPlatformDS;UID=c##colabplatform;PWD=root"

    let student = Student { studentID = 0, studentFirstName = "John3", studentLastName = "Doe", studentEmail = "john@example.com", studentPassword = "mypassword" }

    -- Call insert function for Student
    id <- insert conn student
    print id
    rows <- quickQuery conn "SELECT * FROM STUDENTS" []
    print rows
    commit conn
    disconnect conn