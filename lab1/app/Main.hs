import Database.HDBC
import Database.HDBC.ODBC
import Data.Time.Clock (UTCTime)
import Data.Maybe (mapMaybe)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Format (defaultTimeLocale, parseTimeM)


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

parseCustomDate :: String -> Maybe UTCTime
parseCustomDate dateString = parseTimeM True defaultTimeLocale "%d.%m.%Y" dateString :: Maybe UTCTime

runApp :: Connection -> IO ()
runApp conn = do
    putStrLn "What would you like to do?"
    putStrLn "1. Insert a student"
    putStrLn "2. Get student by ID"
    putStrLn "3. Get all students"
    putStrLn "4. Insert a teacher"
    putStrLn "5. Get teacher by ID"
    putStrLn "6. Get all teachers"
    putStrLn "7. Insert a topic"
    putStrLn "8. Get topic by ID"
    putStrLn "9. Get all topics"
    putStrLn "10. Insert a schedule"
    putStrLn "11. Get schedule by ID"
    putStrLn "12. Get all schedules"
    putStrLn "13. Insert a task"
    putStrLn "14. Grade a task"
    putStrLn "15. Get task by ID"
    putStrLn "16. Get all tasks"
    putStrLn "17. Insert a material"
    putStrLn "18. Get material by ID"
    putStrLn "19. Get all materials"
    putStrLn "20. Insert a question"
    putStrLn "21. Get question by ID"
    putStrLn "22. Update a question"
    putStrLn "23. Get all questions"
    putStrLn "24. Insert an answer"
    putStrLn "25. Get answer by ID"
    putStrLn "26. Update an answer"
    putStrLn "27. Get all answers"
    putStrLn "28. Exit"

    putStrLn "Enter your choice (1-28): "
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
            putStrLn "Enter teacher details:"
            putStrLn "First Name: "
            firstName <- getLine
            putStrLn "Last Name: "
            lastName <- getLine
            putStrLn "Email: "
            email <- getLine
            putStrLn "Password: "
            password <- getLine

            let newTeacher = Teacher { teacherID = Nothing, teacherFirstName = firstName, teacherLastName = lastName, teacherEmail = email, teacherPassword = password }
            insertedId <- insert conn newTeacher
            case insertedId of
                Just id -> putStrLn $ "Inserted teacher with ID: " ++ show id
                Nothing -> putStrLn "Failed to insert teacher"
            runApp conn

        "5" -> do
            putStrLn "Enter teacher ID: "
            teacherIdStr <- getLine
            let teacherId = read teacherIdStr :: Int
            maybeTeacher <- getById conn teacherId :: IO (Maybe Teacher)
            case maybeTeacher of
                Just teacher -> putStrLn $ "Teacher details: " ++ show teacher
                Nothing -> putStrLn "Teacher not found"
            runApp conn

        "6" -> do
            teachers <- getAll conn :: IO [Teacher]
            putStrLn "All teachers: "
            mapM_ (putStrLn . show) teachers
            runApp conn

        "7" -> do
            putStrLn "Enter topic details:"
            putStrLn "Topic Title: "
            title <- getLine
            putStrLn "Topic Description: "
            description <- getLine

            let newTopic = Topic { topicID = Nothing, topicTitle = title, topicDescription = description }
            insertedId <- insert conn newTopic
            case insertedId of
                Just id -> putStrLn $ "Inserted topic with ID: " ++ show id
                Nothing -> putStrLn "Failed to insert topic"
            runApp conn

        "8" -> do
            putStrLn "Enter topic ID: "
            topicIdStr <- getLine
            let topicId = read topicIdStr :: Int
            maybeTopic <- getById conn topicId :: IO (Maybe Topic)
            case maybeTopic of
                Just topic -> putStrLn $ "Topic details: " ++ show topic
                Nothing -> putStrLn "Topic not found"
            runApp conn

        "9" -> do
            topics <- getAll conn :: IO [Topic]
            putStrLn "All topics: "
            mapM_ (putStrLn . show) topics
            runApp conn

        "10" -> do
            putStrLn "Enter schedule details:"
            putStrLn "Topic ID: "
            topicIdStr <- getLine
            let topicId = read topicIdStr :: Int

            putStrLn "Teacher ID: "
            teacherIdStr <- getLine
            let teacherId = read teacherIdStr :: Int

            putStrLn "Start Date: "
            startDateStr <- getLine
            let startDateMaybe = parseCustomDate startDateStr
            let startDate = maybe (error "Invalid start date format") id startDateMaybe

            putStrLn "End Date: "
            endDateStr <- getLine
            let endDateMaybe = parseCustomDate endDateStr
            let endDate = maybe (error "Invalid end date format") id endDateMaybe

            let newSchedule = Schedule { scheduleID = Nothing, scheduleTopicID = topicId, scheduleTeacherID = teacherId, scheduleStartDate = startDate, scheduleEndDate = endDate }
            insertedId <- insert conn newSchedule
            case insertedId of
                Just id -> putStrLn $ "Inserted schedule with ID: " ++ show id
                Nothing -> putStrLn "Failed to insert schedule"
            runApp conn

        "11" -> do
           putStrLn "Enter schedule ID: "
           scheduleIdStr <- getLine
           let scheduleId = read scheduleIdStr :: Int
           maybeSchedule <- getById conn scheduleId :: IO (Maybe Schedule)
           case maybeSchedule of
               Just schedule -> putStrLn $ "Schedule details: " ++ show schedule
               Nothing -> putStrLn "Schedule not found"
           runApp conn

        "12" -> do
           schedules <- getAll conn :: IO [Schedule]
           putStrLn "All schedules: "
           mapM_ (putStrLn . show) schedules
           runApp conn
        "13" -> do
            putStrLn "Enter task details:"
            putStrLn "Student ID: "
            studentIdStr <- getLine
            let studentId = read studentIdStr :: Int

            putStrLn "Topic ID: "
            topicIdStr <- getLine
            let topicId = read topicIdStr :: Int

            putStrLn "Description: "
            description <- getLine

            let newTask = Task { taskID = Nothing, taskStudentID = studentId, taskTopicID = topicId, taskDescription = description, taskGrade = Nothing }
            insertedId <- insert conn newTask
            case insertedId of
                Just id -> putStrLn $ "Inserted task with ID: " ++ show id
                Nothing -> putStrLn "Failed to insert task"
            runApp conn

        "14" -> do
            putStrLn "Task ID: "
            taskIdStr <- getLine
            let taskId = read taskIdStr :: Int
            putStrLn "Enter task grade:"
            gradeStr <- getLine
            let grade = read gradeStr :: Int
            gradeTask conn taskId grade
            putStrLn $ "Graded!"
            runApp conn

        "15" -> do
            putStrLn "Enter task ID: "
            taskIdStr <- getLine
            let taskId = read taskIdStr :: Int
            maybeTask <- getById conn taskId :: IO (Maybe Task)
            case maybeTask of
                Just task -> putStrLn $ "Task details: " ++ show task
                Nothing -> putStrLn "Task not found"
            runApp conn

        "16" -> do
            tasks <- getAll conn :: IO [Task]
            putStrLn "All tasks: "
            mapM_ (putStrLn . show) tasks
            runApp conn

        "17" -> do
            putStrLn "Enter material details:"
            putStrLn "Topic ID: "
            topicIdStr <- getLine
            let topicId = read topicIdStr :: Int

            putStrLn "Title: "
            title <- getLine

            putStrLn "Link: "
            link <- getLine

            let newMaterial = Material { materialID = Nothing, materialTopicID = topicId, materialTitle = title, materialLink = link }
            insertedId <- insert conn newMaterial
            case insertedId of
                Just id -> putStrLn $ "Inserted material with ID: " ++ show id
                Nothing -> putStrLn "Failed to insert material"
            runApp conn

        "18" -> do
            putStrLn "Enter material ID: "
            materialIdStr <- getLine
            let materialId = read materialIdStr :: Int
            maybeMaterial <- getById conn materialId :: IO (Maybe Material)
            case maybeMaterial of
                Just material -> putStrLn $ "Material details: " ++ show material
                Nothing -> putStrLn "Material not found"
            runApp conn

        "19" -> do
            materials <- getAll conn :: IO [Material]
            putStrLn "All materials: "
            mapM_ (putStrLn . show) materials
            runApp conn

        "20" -> do
            putStrLn "Enter question details:"
            putStrLn "Student ID: "
            studentIdStr <- getLine
            let studentId = read studentIdStr :: Int

            putStrLn "Text: "
            text <- getLine

            let newQuestion = Question { questionID = Nothing, questionStudentID = studentId, questionText = text }
            insertedId <- insert conn newQuestion
            case insertedId of
                Just id -> putStrLn $ "Inserted question with ID: " ++ show id
                Nothing -> putStrLn "Failed to insert question"
            runApp conn

        "21" -> do
            putStrLn "Enter question ID: "
            questionIdStr <- getLine
            let questionId = read questionIdStr :: Int
            maybeQuestion <- getById conn questionId :: IO (Maybe Question)
            case maybeQuestion of
                Just question -> putStrLn $ "Question details: " ++ show question
                Nothing -> putStrLn "Question not found"
            runApp conn

        "22" -> do
            putStrLn "Enter question ID to update: "
            questionIdStr <- getLine
            let questionId = read questionIdStr :: Int

            putStrLn "New Text: "
            newText <- getLine

            let updatedQuestion = Question { questionID = Just questionId, questionStudentID = 0, questionText = newText }
            update conn updatedQuestion
            putStrLn "Question updated"
            runApp conn

        "23" -> do
            questions <- getAll conn :: IO [Question]
            putStrLn "All questions: "
            mapM_ (putStrLn . show) questions
            runApp conn

        "24" -> do
            putStrLn "Enter answer details:"
            putStrLn "Question ID: "
            questionIdStr <- getLine
            let questionId = read questionIdStr :: Int

            putStrLn "Teacher ID: "
            teacherIdStr <- getLine
            let teacherId = read teacherIdStr :: Int

            putStrLn "Answer Text: "
            text <- getLine

            let newAnswer = Answer { answerID = Nothing, answerQuestionID = questionId, answerTeacherID = teacherId, answerText = text }
            insertedId <- insert conn newAnswer
            case insertedId of
                Just id -> putStrLn $ "Inserted answer with ID: " ++ show id
                Nothing -> putStrLn "Failed to insert answer"
            runApp conn

        "25" -> do
            putStrLn "Enter answer ID: "
            answerIdStr <- getLine
            let answerId = read answerIdStr :: Int
            maybeAnswer <- getById conn answerId :: IO (Maybe Answer)
            case maybeAnswer of
                Just answer -> putStrLn $ "Answer details: " ++ show answer
                Nothing -> putStrLn "Answer not found"
            runApp conn

        "26" -> do
            putStrLn "Enter answer ID to update: "
            answerIdStr <- getLine
            let answerId = read answerIdStr :: Int

            putStrLn "New Answer Text: "
            newText <- getLine

            let updatedAnswer = Answer { answerID = Just answerId, answerQuestionID = 0, answerTeacherID = 0, answerText = newText }
            update conn updatedAnswer
            putStrLn "Answer updated"
            runApp conn

        "27" -> do
          answers <- getAll conn :: IO [Answer]
          putStrLn "All answers: "
          mapM_ (putStrLn . show) answers
          runApp conn

        "28" -> do
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