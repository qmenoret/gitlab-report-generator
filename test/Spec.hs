{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Control.Exception (evaluate)

-- Ext libs
import Data.Aeson
import Data.List
import Data.Ord
import Data.Maybe

-- My lib
import Lib 

-- Import md generator
import Markdown

-- Import Parsing submodules
import ComplexList
import qualified User as U
import qualified Milestone as M
import qualified Task as T

-- Import Interactions
import qualified ParserConfig as C

main :: IO ()
main = hspec $ do
    describe "JSON parser for tasks" $ do
        it "try parsing user" $ do
            let jsonUser = "{\"name\": \"James Bond\",\"username\": \"jbond\",\"id\": 180,\"state\": \"active\",\"avatar_url\": \"https://my-gitlab.com/uploads/user/avatar/180/avatar.png\",\"web_url\": \"https://my-gitlab.com/jbond\"}"
            let Just u = decode jsonUser :: Maybe U.User
            U.name              u   `shouldBe`  "James Bond"
            U.username          u   `shouldBe`  "jbond"
            U.id                u   `shouldBe`  180
            U.state             u   `shouldBe`  "active"
            U.avatar_url        u   `shouldBe`  "https://my-gitlab.com/uploads/user/avatar/180/avatar.png"
            U.web_url           u   `shouldBe`  "https://my-gitlab.com/jbond"

        it "try parsing milestone" $ do
            let jsonMilestone = "{\"id\": 1,\"iid\": 2,\"project_id\": 43,\"title\": \"v1.0\",\"description\": \"Hello world\",\"state\": \"active\",\"created_at\": \"2017-02-22T13:12:49.640+01:00\",\"updated_at\": \"2017-02-22T13:12:49.640+01:00\",\"due_date\": \"2017-04-07\",\"start_date\": null}"
            let Just m = decode jsonMilestone :: Maybe M.Milestone
            M.id                m   `shouldBe`  1
            M.iid               m   `shouldBe`  2
            M.project_id        m   `shouldBe`  43 
            M.title             m   `shouldBe`  "v1.0"
            M.description       m   `shouldBe`  Just "Hello world"
            M.state             m   `shouldBe`  "active"
            M.created_at        m   `shouldBe`  "2017-02-22T13:12:49.640+01:00"
            M.updated_at        m   `shouldBe`  "2017-02-22T13:12:49.640+01:00"
            M.due_date          m   `shouldBe`  Just "2017-04-07"
            M.start_date        m   `shouldBe`  Nothing

        it "try parsing task" $ do
            let Just t = decode jsonTask :: Maybe T.Task
            T.id                t   `shouldBe`  1
            T.iid               t   `shouldBe`  49
            T.project_id        t   `shouldBe`  43
            T.title             t   `shouldBe`  "Super-Issue-Name"
            T.description       t   `shouldBe`  Just "issue description"
            T.state             t   `shouldBe`  "opened"
            T.created_at        t   `shouldBe`  "2017-02-23T10:23:32.218+01:00"
            T.updated_at        t   `shouldBe`  "2017-02-23T10:24:02.492+01:00"
            T.labels            t   `shouldBe`  ["Label1","To Do"]

            let Just m = T.milestone t
            M.id                m   `shouldBe`  1
            M.iid               m   `shouldBe`  2
            M.project_id        m   `shouldBe`  43
            M.title             m   `shouldBe`  "v1.0"
            M.description       m   `shouldBe`  Just "Hello world"
            M.state             m   `shouldBe`  "active"
            M.created_at        m   `shouldBe`  "2017-02-22T13:12:49.640+01:00"
            M.updated_at        m   `shouldBe`  "2017-02-22T13:12:49.640+01:00"
            M.due_date          m   `shouldBe`  Just "2017-04-07"
            M.start_date        m   `shouldBe`  Nothing

            T.assignee          t   `shouldBe` Nothing 

            let u = T.author t
            U.name              u   `shouldBe`  "James Bond"
            U.username          u   `shouldBe`  "jbond"
            U.id                u   `shouldBe`  180
            U.state             u   `shouldBe`  "active"
            U.avatar_url        u   `shouldBe`  "https://my-gitlab.com/uploads/user/avatar/180/avatar.png"
            U.web_url           u   `shouldBe`  "https://my-gitlab.com/jbond"

            T.subscribed        t   `shouldBe`  False
            T.user_notes_count  t   `shouldBe`  0
            T.upvotes           t   `shouldBe`  0
            T.downvotes         t   `shouldBe`  0
            T.due_date          t   `shouldBe`  Nothing
            T.confidential      t   `shouldBe`  False
            T.web_url           t   `shouldBe`  "https://my-gitlab.com/Group/ProjectName/issues/1"

        it "Milestone can be null" $ do
            let Just t = decode issueAlone :: Maybe T.Task
            T.milestone t `shouldBe` Nothing

        it "Try parsing list of tasks" $ do
            let Just tasks = decode issues :: Maybe T.Tasks
            length tasks `shouldBe` 4

    describe "Try generate md" $ do
        it "Generate table of strings" $ do
            table [["a", "b"],["","Long one"],["String","Yep"]] `shouldBe` "|a|b|\n|---|---|\n| |Long one|\n|String|Yep|\n"
            table [["a", "b", "loooong header"],["",show True,show 3],["String",show False,show (-4)]] `shouldBe` "|a|b|loooong header|\n|---|---|---|\n| |True|3|\n|String|False|-4|\n"


    describe "Sort and filter tasks" $ do
        let Just [t1,t2,t3,t4] = decode issues :: Maybe T.Tasks
        let tasks = [t4,t3,t2,t1]

        it "Default (useless) sort" $ do
            sort tasks `shouldBe` [t1,t2,t3,t4]

        it "sortOn works on one key (composed)" $ do
            sortOn T.title tasks `shouldBe` [t4,t1,t3,t2]
            sortOn (M.title . fromJust . T.milestone) tasks `shouldBe` [t4,t3,t2,t1]

        it "sort on several keys" $ do
            complexSort [ (comparing T.title) ] tasks `shouldBe` [t4,t1,t3,t2]
            complexSort [ (comparing (M.title . fromJust . T.milestone))
                        , (comparing T.title)
                    ] tasks `shouldBe` [t4,t3,t1,t2]

        it "sort on several types" $ do
            complexSort [ (comparing (M.title . fromJust . T.milestone))
                        , (comparing T.id)
                        , (comparing T.confidential)
                    ] tasks `shouldBe` [t4,t3,t1,t2]

        it "Filter tasks" $ do
            complexFilter [ (\t -> T.id t > 2) ] tasks `shouldBe` [t4,t3]
            complexFilter [ (\t -> T.id t > 1)
                          , (\t -> (M.state . fromJust . T.milestone) t == "active")
                ] tasks `shouldBe` [t3,t2]

        it "Filter and sort chaining !" $ do
            (complexSort [comparing T.title] . complexFilter [(\t -> (M.state . fromJust . T.milestone) t == "active")]) tasks
                `shouldBe` [t1,t3,t2]

    describe "Use strings to filter and sort" $ do
        let Just [t1,t2,t3,t4] = decode issues :: Maybe T.Tasks
        let tasks = [t4,t3,t2,t1]
        
        it "sort on several keys" $ do
            complexSort (T.getComparators ["title"]) tasks `shouldBe` [t4,t1,t3,t2]
            complexSort (T.getComparators ["author.username"]) tasks `shouldBe` [t1,t4,t2,t3]
            complexSort (T.getComparators ["milestone.title","id"]) tasks `shouldBe` [t4,t3,t1,t2]
            complexSort (T.getComparators ["milestone.title","title"]) tasks `shouldBe` [t4,t3,t1,t2]
        
        it "reverse sort on several keys" $ do
            complexSort (T.getComparators ["#title"]) tasks `shouldBe` reverse [t4,t1,t3,t2]
            complexSort (T.getComparators ["#author.username"]) tasks `shouldBe` reverse [t1,t4,t2,t3]
            complexSort (T.getComparators ["#milestone.title","#id"]) tasks `shouldBe` reverse [t4,t3,t1,t2]
            complexSort (T.getComparators ["#milestone.title","#title"]) tasks `shouldBe` reverse [t4,t3,t1,t2]

        it "filters" $ do
            (complexSort (T.getComparators ["title"]) . complexFilter (T.getFilters ["open"])) tasks `shouldBe` [t4,t1,t3,t2]
            (complexSort (T.getComparators ["title"]) . complexFilter (T.getFilters ["assigned", "open"])) tasks `shouldBe` []

        it "generate dynamic filter" $ do
            complexFilter (T.getFilters ["state=opened"])   tasks `shouldBe`    [t4,t3,t2,t1]
            complexFilter (T.getFilters ["state=closed"])   tasks `shouldBe`    []
            complexFilter (T.getFilters ["state~closed"])   tasks `shouldBe`    [t4,t3,t2,t1]
            complexFilter (T.getFilters ["assignee~"])      tasks `shouldBe`    []
            complexFilter (T.getFilters ["id>2"])           tasks `shouldBe`    [t4,t3]

        it "generate dynamic filter on subclass" $ do
            complexFilter (T.getFilters ["milestone.state=active"])   tasks `shouldBe` [t3,t2,t1]

        it "generate reverse dynamic filter" $ do
            complexFilter (T.getFilters ["#id>2"])           tasks `shouldBe`    [t2,t1]

    describe "Parse program arguments" $ do
        it "Sweet default config" $ do
            C.input C.defaultParserConfig     `shouldBe`  C.FromStdin
            C.filters C.defaultParserConfig   `shouldBe`  []
            C.sortKeys C.defaultParserConfig  `shouldBe`  ["title"]
            
        it "Update existing conf from args" $ do
            C.parseArgs C.defaultParserConfig []                        `shouldBe` C.defaultParserConfig
            C.parseArgs C.defaultParserConfig ["--input-file", "toto" ] `shouldBe` C.defaultParserConfig { C.input    = C.FromFile "toto" }
            C.parseArgs C.defaultParserConfig ["--filters",    "f1,f2"] `shouldBe` C.defaultParserConfig { C.filters  = ["f1","f2"] }
            C.parseArgs C.defaultParserConfig ["--sort-keys",  "s1,s2"] `shouldBe` C.defaultParserConfig { C.sortKeys = ["s1","s2"] }
            C.parseArgs C.defaultParserConfig ["--sort-keys",  "s1,s2"
                                        ,"--filters",    "f1,f2"
                                        ,"--input-file", "toto" ] `shouldBe` C.defaultParserConfig { C.filters  = ["f1","f2"]
                                                                                             , C.sortKeys = ["s1","s2"]
                                                                                             , C.input    = C.FromFile "toto" }

        it "Generate new conf from cmd args" $ do
            C.fromArgs ["--input-file", "toto", "--filters", "myf,myotherf", "--sort-keys", "title"]
                `shouldBe` C.defaultParserConfig  { C.filters  = ["myf","myotherf"]
                                            , C.sortKeys = ["title"]
                                            , C.input    = C.FromFile "toto" }
            
        it "Bad arg throws" $ do
            evaluate (C.fromArgs ["idontexist"]) `shouldThrow` anyException
            


































--
-- DATA
--
--

jsonTask = "{\"id\":1,\"iid\":49,\"project_id\":43,\"title\":\"Super-Issue-Name\",\"description\":\"issue description\",\"state\":\"opened\",\"created_at\":\"2017-02-23T10:23:32.218+01:00\",\"updated_at\":\"2017-02-23T10:24:02.492+01:00\",\"labels\":[\"Label1\",\"To Do\"],\"milestone\":{\"id\":1,\"iid\":2,\"project_id\":43,\"title\":\"v1.0\",\"description\":\"Hello world\",\"state\":\"active\",\"created_at\":\"2017-02-22T13:12:49.640+01:00\",\"updated_at\":\"2017-02-22T13:12:49.640+01:00\",\"due_date\":\"2017-04-07\",\"start_date\":null},\"assignee\":null,\"author\":{\"name\":\"James Bond\",\"username\":\"jbond\",\"id\":180,\"state\":\"active\",\"avatar_url\":\"https://my-gitlab.com/uploads/user/avatar/180/avatar.png\",\"web_url\":\"https://my-gitlab.com/jbond\"},\"subscribed\":false,\"user_notes_count\":0,\"upvotes\":0,\"downvotes\":0,\"due_date\":null,\"confidential\":false,\"web_url\":\"https://my-gitlab.com/Group/ProjectName/issues/1\"}"
issueAlone = "{\"id\":1,\"iid\":49,\"project_id\":43,\"title\":\"Super-Issue-Name\",\"description\":\"issue description\",\"state\":\"opened\",\"created_at\":\"2017-02-23T10:23:32.218+01:00\",\"updated_at\":\"2017-02-23T10:24:02.492+01:00\",\"labels\":[\"Label1\",\"To Do\"],\"milestone\":null,\"assignee\":null,\"author\":{\"name\":\"James Bond\",\"username\":\"jbond\",\"id\":180,\"state\":\"active\",\"avatar_url\":\"https://my-gitlab.com/uploads/user/avatar/180/avatar.png\",\"web_url\":\"https://my-gitlab.com/jbond\"},\"subscribed\":false,\"user_notes_count\":0,\"upvotes\":0,\"downvotes\":0,\"due_date\":null,\"confidential\":false,\"web_url\":\"https://my-gitlab.com/Group/ProjectName/issues/1\"}"




issues = "[{\"id\":1,\"iid\":49,\"project_id\":43,\"title\":\"Good-Issue-Name\",\"description\":\"issue description\",\"state\":\"opened\",\"created_at\":\"2017-02-23T10:23:32.218+01:00\",\"updated_at\":\"2017-02-23T10:24:02.492+01:00\",\"labels\":[\"Label1\",\"To Do\"],\"milestone\":{\"id\":1,\"iid\":2,\"project_id\":43,\"title\":\"v1.0\",\"description\":\"Hello world\",\"state\":\"active\",\"created_at\":\"2017-02-22T13:12:49.640+01:00\",\"updated_at\":\"2017-02-22T13:12:49.640+01:00\",\"due_date\":\"2017-04-07\",\"start_date\":null},\"assignee\":null,\"author\":{\"name\":\"James Bond\",\"username\":\"jbond\",\"id\":180,\"state\":\"active\",\"avatar_url\":\"https://my-gitlab.com/uploads/user/avatar/180/avatar.png\",\"web_url\":\"https://my-gitlab.com/jbond\"},\"subscribed\":false,\"user_notes_count\":0,\"upvotes\":0,\"downvotes\":0,\"due_date\":null,\"confidential\":false,\"web_url\":\"https://my-gitlab.com/Group/ProjectName/issues/1\"},{\"id\":2,\"iid\":50,\"project_id\":43,\"title\":\"Super-Issue-Name2\",\"description\":\"issue description T_T\",\"state\":\"opened\",\"created_at\":\"2017-02-23T10:23:32.218+01:00\",\"updated_at\":\"2017-02-23T10:24:02.492+01:00\",\"labels\":[\"Label3\",\"Doing\"],\"milestone\":{\"id\":1,\"iid\":2,\"project_id\":43,\"title\":\"v1.0\",\"description\":\"Hello world\",\"state\":\"active\",\"created_at\":\"2017-02-22T13:12:49.640+01:00\",\"updated_at\":\"2017-02-22T13:12:49.640+01:00\",\"due_date\":\"2017-04-14\",\"start_date\":null},\"assignee\":null,\"author\":{\"name\":\"Linus Torvald\",\"username\":\"ltorvald\",\"id\":180,\"state\":\"active\",\"avatar_url\":\"https://my-gitlab.com/uploads/user/avatar/180/avatar.png\",\"web_url\":\"https://my-gitlab.com/jbond\"},\"subscribed\":false,\"user_notes_count\":0,\"upvotes\":0,\"downvotes\":0,\"due_date\":null,\"confidential\":false,\"web_url\":\"https://my-gitlab.com/Group/ProjectName/issues/1\"},{\"id\":3,\"iid\":51,\"project_id\":43,\"title\":\"MyIssue\",\"description\":\"issue description\",\"state\":\"opened\",\"created_at\":\"2017-02-23T10:23:32.218+01:00\",\"updated_at\":\"2017-02-23T10:24:02.492+01:00\",\"labels\":[\"Label1\",\"To Do\"],\"milestone\":{\"id\":2,\"iid\":2,\"project_id\":43,\"title\":\"Beta\",\"description\":\"Hello world\",\"state\":\"active\",\"created_at\":\"2017-02-22T13:12:49.640+01:00\",\"updated_at\":\"2017-02-22T13:12:49.640+01:00\",\"due_date\":\"2017-04-07\",\"start_date\":null},\"assignee\":null,\"author\":{\"name\":\"Mama Mia\",\"username\":\"mmia\",\"id\":180,\"state\":\"active\",\"avatar_url\":\"https://my-gitlab.com/uploads/user/avatar/180/avatar.png\",\"web_url\":\"https://my-gitlab.com/jbond\"},\"subscribed\":false,\"user_notes_count\":0,\"upvotes\":0,\"downvotes\":0,\"due_date\":null,\"confidential\":false,\"web_url\":\"https://my-gitlab.com/Group/ProjectName/issues/1\"},{\"id\":4,\"iid\":52,\"project_id\":43,\"title\":\"A is first letter\",\"description\":\"issue description\",\"state\":\"opened\",\"created_at\":\"2017-02-23T10:23:32.218+01:00\",\"updated_at\":\"2017-02-23T10:24:02.492+01:00\",\"labels\":[\"Label1\",\"To Do\"],\"milestone\":{\"id\":3,\"iid\":2,\"project_id\":43,\"title\":\"Alpha\",\"description\":\"Hello world\",\"state\":\"inactive\",\"created_at\":\"2017-02-22T13:12:49.640+01:00\",\"updated_at\":\"2017-02-22T13:12:49.640+01:00\",\"due_date\":\"2017-04-07\",\"start_date\":null},\"assignee\":null,\"author\":{\"name\":\"Kevin Ata\",\"username\":\"kata\",\"id\":180,\"state\":\"active\",\"avatar_url\":\"https://my-gitlab.com/uploads/user/avatar/180/avatar.png\",\"web_url\":\"https://my-gitlab.com/jbond\"},\"subscribed\":false,\"user_notes_count\":0,\"upvotes\":0,\"downvotes\":0,\"due_date\":null,\"confidential\":false,\"web_url\":\"https://my-gitlab.com/Group/ProjectName/issues/1\"}]"
