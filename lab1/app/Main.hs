import Database.HDBC
import Database.HDBC.ODBC
import Data.Time.Clock (UTCTime)
import Data.Maybe (mapMaybe)

-- Define a class for insertable entities
class Insertable a where
    insert :: Connection -> a -> IO (Maybe Integer)

class Retrievable a where
    getById :: Connection -> Int -> IO (Maybe a)
    getAll :: Connection -> IO [a]

-- Define data types representing each table

data Student = Student {
    studentID :: Maybe Int,
    studentFirstName :: String,
    studentLastName :: String,
    studentEmail :: String,
    studentPassword :: String
} deriving (Show)

data Teacher = Teacher {
    teacherID :: Maybe Int,
    teacherFirstName :: String,
    teacherLastName :: String,
    teacherEmail :: String,
    teacherPassword :: String
} deriving (Show)

data Topic = Topic {
    topicID :: Maybe Int,
    topicTitle :: String,
    topicDescription :: String
} deriving (Show)

data Schedule = Schedule {
    scheduleID :: Maybe Int,
    scheduleTopicID :: Int,
    scheduleTeacherID :: Int,
    scheduleStartDate :: UTCTime,
    scheduleEndDate :: UTCTime
} deriving (Show)

data Task = Task {
    taskID :: Maybe Int,
    taskStudentID :: Int,
    taskTopicID :: Int,
    taskDescription :: String,
    taskGrade :: Maybe Int
} deriving (Show)

data Material = Material {
    materialID :: Maybe Int,
    materialTopicID :: Int,
    materialTitle :: String,
    materialLink :: String
} deriving (Show)

data Question = Question {
    questionID :: Maybe Int,
    questionStudentID :: Int,
    questionText :: String
} deriving (Show)

data Answer = Answer {
    answerID :: Maybe Int,
    answerQuestionID :: Int,
    answerTeacherID :: Int,
    answerText :: String
} deriving (Show)


instance Insertable Student where
    insert conn student = do
      stmt <- prepare conn "INSERT INTO Students (FirstName, LastName, Email, Password) VALUES (?, ?, ?, ?)"
      execute stmt [toSql (studentFirstName student), toSql (studentLastName student), toSql (studentEmail student), toSql (studentPassword student)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT StudentID FROM Students WHERE ROWID = (SELECT MAX(ROWID) FROM Students)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

instance Retrievable Student where
    getById conn id = do
        stmt <- prepare conn "SELECT * FROM Students WHERE StudentID = ?"
        execute stmt [toSql id]
        result <- fetchRow stmt
        return $ maybe Nothing convertToStudent result

    getAll conn = do
        stmt <- prepare conn "SELECT * FROM Students"
        execute stmt []
        results <- fetchAllRows stmt
        return $ mapMaybe convertToStudent results

convertToStudent :: [SqlValue] -> Maybe Student
convertToStudent [sqlId, firstName, lastName, email, password] =
    Just Student
        { studentID = fromSql sqlId
        , studentFirstName = fromSql firstName
        , studentLastName = fromSql lastName
        , studentEmail = fromSql email
        , studentPassword = fromSql password
        }
convertToStudent _ = Nothing

instance Insertable Teacher where
    insert conn teacher = do
      stmt <- prepare conn "INSERT INTO Teachers (FirstName, LastName, Email, Password) VALUES (?, ?, ?, ?)"
      execute stmt [toSql (teacherFirstName teacher), toSql (teacherLastName teacher), toSql (teacherEmail teacher), toSql (teacherPassword teacher)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT TeacherID FROM Teachers WHERE ROWID = (SELECT MAX(ROWID) FROM Teachers)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

instance Retrievable Teacher where
    getById conn id = do
        stmt <- prepare conn "SELECT * FROM Teachers WHERE TeacherID = ?"
        execute stmt [toSql id]
        result <- fetchRow stmt
        return $ maybe Nothing convertToTeacher result

    getAll conn = do
        stmt <- prepare conn "SELECT * FROM Teachers"
        execute stmt []
        results <- fetchAllRows stmt
        return $ mapMaybe convertToTeacher results

convertToTeacher :: [SqlValue] -> Maybe Teacher
convertToTeacher [sqlId, firstName, lastName, email, password] =
                Just Teacher
                    { teacherID = Just (fromSql sqlId)
                    , teacherFirstName = fromSql firstName
                    , teacherLastName = fromSql lastName
                    , teacherEmail = fromSql email
                    , teacherPassword = fromSql password
                    }
convertToTeacher _ = Nothing

instance Insertable Topic where
    insert conn topic = do
      stmt <- prepare conn "INSERT INTO TOPICS (TOPICTITLE, TOPICDESCRIPTION) VALUES (?, ?)"
      execute stmt [toSql (topicTitle topic), toSql (topicDescription topic)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT TOPICID FROM TOPICS WHERE ROWID = (SELECT MAX(ROWID) FROM TOPICS)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

instance Retrievable Topic where
    getById conn id = do
        stmt <- prepare conn "SELECT * FROM Topics WHERE TopicID = ?"
        execute stmt [toSql id]
        result <- fetchRow stmt
        return $ maybe Nothing convertToTopic result

    getAll conn = do
        stmt <- prepare conn "SELECT * FROM Topics"
        execute stmt []
        results <- fetchAllRows stmt
        return $ mapMaybe convertToTopic results

convertToTopic :: [SqlValue] -> Maybe Topic
convertToTopic [sqlId, title, desc] =
    Just Topic
        { topicID = Just (fromSql sqlId)
        , topicTitle = fromSql title
        , topicDescription = fromSql desc
        }
convertToTopic _ = Nothing

instance Insertable Schedule where
    insert conn schedule = do
      stmt <- prepare conn "INSERT INTO Schedule (TopicID, TeacherID, StartDate, EndDate) VALUES (?, ?, ?, ?)"
      execute stmt [toSql (scheduleTopicID schedule), toSql (scheduleTeacherID schedule), toSql (scheduleStartDate schedule), toSql (scheduleEndDate schedule)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT ScheduleID FROM Schedule WHERE ROWID = (SELECT MAX(ROWID) FROM Schedule)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

instance Insertable Task where
    insert conn task = do
      stmt <- prepare conn "INSERT INTO Tasks (STUDENTID, TOPICID, TASKDESCRIPTION) VALUES (?, ?, ?)"
      execute stmt [toSql (taskStudentID task), toSql (taskTopicID task), toSql (taskDescription task)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT TaskID FROM Tasks WHERE ROWID = (SELECT MAX(ROWID) FROM Tasks)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

instance Retrievable Task where
    getById conn id = do
        stmt <- prepare conn "SELECT * FROM Tasks WHERE TaskID = ?"
        execute stmt [toSql id]
        result <- fetchRow stmt
        return $ maybe Nothing (convertToTask . map fromSql) result

    getAll conn = do
        stmt <- prepare conn "SELECT * FROM Tasks"
        execute stmt []
        results <- fetchAllRows stmt
        return $ mapMaybe (convertToTask . map fromSql) results

convertToTask :: [SqlValue] -> Maybe Task
convertToTask [sqlId, sqlStudentId, sqlTopicId, sqlDescription, sqlGrade] =
    let maybeGrade = if sqlGrade == SqlNull
                        then Nothing
                        else Just (fromSql sqlGrade :: Int)
    in Just Task
        { taskID = fromSql sqlId
        , taskStudentID = fromSql sqlStudentId
        , taskTopicID = fromSql sqlTopicId
        , taskDescription = fromSql sqlDescription
        , taskGrade = maybeGrade
        }
convertToTask _ = Nothing

gradeTask :: Connection -> Int -> Int -> IO ()
gradeTask conn taskId grade = do
  stmt <- prepare conn "UPDATE Tasks SET TaskGrade = ? WHERE TaskID = ?"
  execute stmt [toSql grade, toSql taskId]
  commit conn


instance Insertable Material where
    insert conn material = do
      stmt <- prepare conn "INSERT INTO MATERIALS (TOPICID, MATERIALTITLE, MATERIALLINK) VALUES (?, ?, ?)"
      execute stmt [toSql (materialTopicID material), toSql (materialTitle material), toSql (materialLink material)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT MATERIALID FROM MATERIALS WHERE ROWID = (SELECT MAX(ROWID) FROM MATERIALS)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing


instance Insertable Question where
    insert conn question = do
      stmt <- prepare conn "INSERT INTO QUESTIONS (STUDENTID, QUESTION) VALUES (?, ?)"
      execute stmt [toSql (questionStudentID question), toSql (questionText question)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT QUESTIONID FROM QUESTIONS WHERE ROWID = (SELECT MAX(ROWID) FROM QUESTIONS)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

instance Insertable Answer where
    insert conn answer = do
      stmt <- prepare conn "INSERT INTO ANSWERS (QUESTIONID, TEACHERID, ANSWER) VALUES (?, ?, ?)"
      execute stmt [toSql (answerQuestionID answer), toSql (answerTeacherID answer), toSql (answerText answer)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT ANSWERID FROM ANSWERS WHERE ROWID = (SELECT MAX(ROWID) FROM ANSWERS)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

main :: IO ()
main = do
    conn <- connectODBC "DSN=colabPlatformDS;UID=c##colabplatform;PWD=root"

--    let student = Student {studentID = Nothing, studentFirstName = "John4", studentLastName = "Doe", studentEmail = "john@example.com", studentPassword = "mypassword" }

    -- Call insert function for Student
--    id <- insert conn student
--    print id
    maybeStudent <- getById conn 4 :: IO (Maybe Student)
    putStrLn $ "Fetched student: " ++ show maybeStudent
    students <- getAll conn :: IO [Student]
    putStrLn "All students:"
    mapM_ (putStrLn . show) students
--    print (getAll conn :: IO [Student])
--    rows <- quickQuery conn "SELECT * FROM STUDENTS" []
--    print rows
    commit conn
    disconnect conn