import Database.HDBC
import Database.HDBC.ODBC
import Data.Time.Clock (UTCTime)
import Data.Maybe (mapMaybe)
import Data.Maybe (fromMaybe)

-- Define a class for insertable entities
class Insertable a where
    insert :: Connection -> a -> IO (Maybe Integer)

-- Define a class for updatable entities
class Updatable a where
    update :: Connection -> a -> IO ()

-- Define a class for retrievable entities
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

instance Retrievable Schedule where
    getById conn id = do
        stmt <- prepare conn "SELECT * FROM Schedule WHERE ScheduleID = ?"
        execute stmt [toSql id]
        result <- fetchRow stmt
        return $ maybe Nothing convertToSchedule result

    getAll conn = do
        stmt <- prepare conn "SELECT * FROM Schedule"
        execute stmt []
        results <- fetchAllRows stmt
        return $ mapMaybe convertToSchedule results

convertToSchedule :: [SqlValue] -> Maybe Schedule
convertToSchedule [sqlId, sqlTopicID, sqlTeacherID, sqlStartDate, sqlEndDate] =
  Just Schedule
          { scheduleID = Just (fromSql sqlId)
          , scheduleTopicID = fromSql sqlTopicID
          , scheduleTeacherID = fromSql sqlTeacherID
          , scheduleStartDate = fromSql sqlStartDate
          , scheduleEndDate = fromSql sqlEndDate
          }
convertToSchedule _ = Nothing

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
        return $ maybe Nothing convertToTask result

    getAll conn = do
        stmt <- prepare conn "SELECT * FROM Tasks"
        execute stmt []
        results <- fetchAllRows stmt
        return $ mapMaybe convertToTask results

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

instance Retrievable Material where
    getById conn id = do
        stmt <- prepare conn "SELECT * FROM Materials WHERE MaterialID = ?"
        execute stmt [toSql id]
        result <- fetchRow stmt
        return $ maybe Nothing convertToMaterial result

    getAll conn = do
        stmt <- prepare conn "SELECT * FROM Materials"
        execute stmt []
        results <- fetchAllRows stmt
        return $ mapMaybe convertToMaterial results

convertToMaterial :: [SqlValue] -> Maybe Material
convertToMaterial [sqlId, sqlTopicId, sqlTitle, sqlLink] =
    Just Material
        { materialID = fromSql sqlId
        , materialTopicID = fromSql sqlTopicId
        , materialTitle = fromSql sqlTitle
        , materialLink = fromSql sqlLink
        }
convertToMaterial _ = Nothing

instance Insertable Question where
    insert conn question = do
      stmt <- prepare conn "INSERT INTO QUESTIONS (STUDENTID, QUESTION) VALUES (?, ?)"
      execute stmt [toSql (questionStudentID question), toSql (questionText question)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT QUESTIONID FROM QUESTIONS WHERE ROWID = (SELECT MAX(ROWID) FROM QUESTIONS)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

instance Retrievable Question where
    getById conn id = do
        stmt <- prepare conn "SELECT * FROM Questions WHERE QuestionID = ?"
        execute stmt [toSql id]
        result <- fetchRow stmt
        return $ maybe Nothing convertToQuestion result

    getAll conn = do
        stmt <- prepare conn "SELECT * FROM Questions"
        execute stmt []
        results <- fetchAllRows stmt
        return $ mapMaybe convertToQuestion results

convertToQuestion :: [SqlValue] -> Maybe Question
convertToQuestion [sqlId, sqlStudentId, sqlText] =
    Just Question
        { questionID = fromSql sqlId
        , questionStudentID = fromSql sqlStudentId
        , questionText = fromSql sqlText
        }
convertToQuestion _ = Nothing

instance Updatable Question where
    update conn question = do
        stmt <- prepare conn "UPDATE Questions SET QuestionText = ? WHERE QuestionID = ?"
        execute stmt [toSql (questionText question), toSql (fromMaybe 0 $ questionID question)]
        commit conn

instance Insertable Answer where
    insert conn answer = do
      stmt <- prepare conn "INSERT INTO ANSWERS (QUESTIONID, TEACHERID, ANSWER) VALUES (?, ?, ?)"
      execute stmt [toSql (answerQuestionID answer), toSql (answerTeacherID answer), toSql (answerText answer)]
      -- Fetch the last inserted ID through a separate query
      result <- quickQuery conn "SELECT ANSWERID FROM ANSWERS WHERE ROWID = (SELECT MAX(ROWID) FROM ANSWERS)" []
      case result of
          [[sqlId]] -> return $ Just (fromSql sqlId)
          _         -> return Nothing

instance Retrievable Answer where
    getById conn id = do
        stmt <- prepare conn "SELECT * FROM Answers WHERE AnswerID = ?"
        execute stmt [toSql id]
        result <- fetchRow stmt
        return $ maybe Nothing convertToAnswer result

    getAll conn = do
        stmt <- prepare conn "SELECT * FROM Answers"
        execute stmt []
        results <- fetchAllRows stmt
        return $ mapMaybe convertToAnswer results

instance Updatable Answer where
    update conn answer = do
        stmt <- prepare conn "UPDATE Answers SET AnswerText = ? WHERE AnswerID = ?"
        execute stmt [ toSql (answerText answer)
                     , toSql (fromMaybe 0 $ answerID answer)
                     ]
        commit conn

convertToAnswer :: [SqlValue] -> Maybe Answer
convertToAnswer [sqlId, sqlQuestionId, sqlTeacherId, sqlText] =
    Just Answer
        { answerID = fromSql sqlId
        , answerQuestionID = fromSql sqlQuestionId
        , answerTeacherID = fromSql sqlTeacherId
        , answerText = fromSql sqlText
        }
convertToAnswer _ = Nothing

runApp :: Connection -> IO ()
runApp conn = do
    putStrLn "What would you like to do?"
    putStrLn "1. Insert a student"
    putStrLn "2. Get student by ID"
    putStrLn "3. Get all students"
    putStrLn "4. Exit"
    putStrLn "Enter your choice (1/2/3/4): "
    choice <- getLine

    case choice of
        "1" -> do
            putStrLn "Enter student details:"
            putStrLn "First Name: "
            firstName <- getLine
            putStrLn "Last Name: "
            lastName <- getLine
            putStrLn "Email: "
            email <- getLine
            putStrLn "Password: "
            password <- getLine

            let newStudent = Student { studentID = Nothing, studentFirstName = firstName, studentLastName = lastName, studentEmail = email, studentPassword = password }
            insertedId <- insert conn newStudent
            case insertedId of
                Just id -> putStrLn $ "Inserted student with ID: " ++ show id
                Nothing -> putStrLn "Failed to insert student"
            runApp conn

        "2" -> do
            putStrLn "Enter student ID: "
            studentIdStr <- getLine
            let studentId = read studentIdStr :: Int
            maybeStudent <- getById conn studentId :: IO (Maybe Student)
            case maybeStudent of
                Just student -> putStrLn $ "Student details: " ++ show student
                Nothing -> putStrLn "Student not found"
            runApp conn

        "3" -> do
            students <- getAll conn :: IO [Student]
            putStrLn "All students: "
            mapM_ (putStrLn . show) students
            runApp conn

        "4" -> do
            putStrLn "Exiting..."
            disconnect conn

        _ -> do
            putStrLn "Invalid choice"
            runApp conn

main :: IO ()
main = do
    putStrLn "Welcome to the Induvidial Student Work Database App!"
    conn <- connectODBC "DSN=colabPlatformDS;UID=c##colabplatform;PWD=root"
    runApp conn