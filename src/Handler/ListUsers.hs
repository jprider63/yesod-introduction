{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.ListUsers where

import Import

getListUsersR :: Handler Html
getListUsersR = do
    users <- runDB $ do
        selectList [] []

    defaultLayout [whamlet|
        <h1>
            Users
        <table>
            ^{mconcat $ map displayUserRow users}
    |]


    where
        displayUserRow (Entity userId user) = [whamlet|
            <tr>
                <td>
                    <a href="@{ProfileR $ userUsername user}">
                        #{userUsername user}
                <td>
                    #{email}
        |]

            where
                email = maybe "no email" id $ userEmail user

