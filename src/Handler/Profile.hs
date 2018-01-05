{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Data.Time.Clock
import Yesod.Form.Bootstrap3

import Import

pokeForm :: Form ()
pokeForm = renderBootstrap3 BootstrapBasicForm $ pure ()

postPokeR :: Text -> Handler Html
postPokeR username = do
    pokerUserId <- requireAuthId
    ((res, form), enctype) <- runFormPost pokeForm
    case res of
        FormSuccess () -> do
            (Entity pokedUserId _) <- runDB $ getBy404 $ UniqueUser username

            now <- liftIO getCurrentTime 
            let poke = Poke pokerUserId pokedUserId now
            runDB $ insert_ poke

            setMessage $ toHtml $ "Poked " <> username

            redirect $ ProfileR username
        _ ->
            -- Error case.
            generateHtml username $ Just (form, enctype)


getProfileR :: Text -> Handler Html
getProfileR username = 
    generateHtml username Nothing

generateHtml username formM = do
    (Entity userId user) <- runDB $ getBy404 $ UniqueUser username
    let email = maybe "-" id $ userEmail user
    pokes <- runDB $ selectList [PokePokee ==. userId] []
    let pokesW = concatMap renderPoke pokes
    defaultLayout $ do
        setTitle $ toHtml $ username <> " Profile"
        [whamlet|
            <a href="@{HomeR}">
                Homepage
            <h1>
                Profile: #{username}
            <div>
                Email: #{email}
            ^{formW}
            <h2>
                Poked by:
            <ul>
                ^{pokesW}
        |]

    where
        formW = do
            userM <- handlerToWidget maybeAuthId
            case userM of
                Nothing ->
                    mempty
                Just _ -> do 
                    (form, enctype) <- handlerToWidget makeForm
                    [whamlet|
                        <form .form-basic role=form method=post action="@{PokeR username}" enctype=#{enctype}>
                            ^{form}
                            <div .form-group>
                                <button .btn .btn-primary type="submit">
                                    Poke
                    |]
        
        makeForm = case formM of
            Nothing -> 
                generateFormPost pokeForm
            Just f -> 
                return f

        renderPoke (Entity _ (Poke poker pokee timestamp)) = 
            [whamlet|
                <li>
                    #{show poker} at #{show timestamp}
            |]

