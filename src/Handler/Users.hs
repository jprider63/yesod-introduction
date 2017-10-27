module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = do
    -- Return all users from database.
    (users :: [Entity User]) <- runDB $ selectList [] [Asc UserUsername]

    let usersW = concatMap userWidget users
    defaultLayout [whamlet|
        <h1>
            Users
        <ul>
            ^{usersW}
    |]

    where
        userWidget (Entity _ user) = [whamlet|
            <li>
                <a href="@{ProfileR $ userUsername user}">
                    #{userUsername user}
        |]
