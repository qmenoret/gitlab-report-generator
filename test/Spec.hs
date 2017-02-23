{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Data.Aeson
import Lib 

-- Import submodules
import qualified User as U
import qualified Milestone as M
import qualified Task as T
-- import qualified User

main :: IO ()
main = hspec $ do
    describe "JSON parser for tasks" $ do
        it "try parsing user" $ do
            let user = "{\"name\": \"James Bond\",\"username\": \"jbond\",\"id\": 180,\"state\": \"active\",\"avatar_url\": \"https://my-gitlab.com/uploads/user/avatar/180/avatar.png\",\"web_url\": \"https://my-gitlab.com/jbond\"}"
            let Just u = decode user :: Maybe U.User
            U.name              u   `shouldBe`  "James Bond"
            U.username          u   `shouldBe`  "jbond"
            U.id                u   `shouldBe`  180
            U.state             u   `shouldBe`  "active"
            U.avatar_url        u   `shouldBe`  "https://my-gitlab.com/uploads/user/avatar/180/avatar.png"
            U.web_url           u   `shouldBe`  "https://my-gitlab.com/jbond"

        it "try parsing milestone" $ do
            let milestone = "{\"id\": 1,\"iid\": 2,\"project_id\": 43,\"title\": \"v1.0\",\"description\": \"Hello world\",\"state\": \"active\",\"created_at\": \"2017-02-22T13:12:49.640+01:00\",\"updated_at\": \"2017-02-22T13:12:49.640+01:00\",\"due_date\": \"2017-04-07\",\"start_date\": null}"
            let Just m = decode milestone :: Maybe M.Milestone
            M.id                m   `shouldBe`  1
            M.iid               m   `shouldBe`  2
            M.project_id        m   `shouldBe`  43 
            M.title             m   `shouldBe`  "v1.0"
            M.description       m   `shouldBe`  "Hello world"
            M.state             m   `shouldBe`  "active"
            M.created_at        m   `shouldBe`  "2017-02-22T13:12:49.640+01:00"
            M.updated_at        m   `shouldBe`  "2017-02-22T13:12:49.640+01:00"
            M.due_date          m   `shouldBe`  Just "2017-04-07"
            M.start_date        m   `shouldBe`  Nothing

        it "try parsing task" $ do
            let task = "{\"id\":1,\"iid\":49,\"project_id\":43,\"title\":\"Super-Issue-Name\",\"description\":\"issue description\",\"state\":\"opened\",\"created_at\":\"2017-02-23T10:23:32.218+01:00\",\"updated_at\":\"2017-02-23T10:24:02.492+01:00\",\"labels\":[\"Label1\",\"To Do\"],\"milestone\":{\"id\":1,\"iid\":2,\"project_id\":43,\"title\":\"v1.0\",\"description\":\"Hello world\",\"state\":\"active\",\"created_at\":\"2017-02-22T13:12:49.640+01:00\",\"updated_at\":\"2017-02-22T13:12:49.640+01:00\",\"due_date\":\"2017-04-07\",\"start_date\":null},\"assignee\":null,\"author\":{\"name\":\"James Bond\",\"username\":\"jbond\",\"id\":180,\"state\":\"active\",\"avatar_url\":\"https://my-gitlab.com/uploads/user/avatar/180/avatar.png\",\"web_url\":\"https://my-gitlab.com/jbond\"},\"subscribed\":false,\"user_notes_count\":0,\"upvotes\":0,\"downvotes\":0,\"due_date\":null,\"confidential\":false,\"web_url\":\"https://my-gitlab.com/Group/ProjectName/issues/1\"}"
            let Just t = decode task :: Maybe T.Task
            T.id                t   `shouldBe`  1
            T.iid               t   `shouldBe`  49
            T.project_id        t   `shouldBe`  43
            T.title             t   `shouldBe`  "Super-Issue-Name"
            T.description       t   `shouldBe`  "issue description"
            T.state             t   `shouldBe`  "opened"
            T.created_at        t   `shouldBe`  "2017-02-23T10:23:32.218+01:00"
            T.updated_at        t   `shouldBe`  "2017-02-23T10:24:02.492+01:00"
            T.labels            t   `shouldBe`  ["Label1","To Do"]

            let m = T.milestone t
            M.id                m   `shouldBe`  1
            M.iid               m   `shouldBe`  2
            M.project_id        m   `shouldBe`  43
            M.title             m   `shouldBe`  "v1.0"
            M.description       m   `shouldBe`  "Hello world"
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

