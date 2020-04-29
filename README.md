# Haskbot

Telegram bot for Haskellers.
This is a work in progress, not ready for usage yet.

![Screen1](img/screen1.png)

## Warning

I’m still a Haskell beginner, so I may do things wrong.

## Building

I hope you won’t try to build it locally, but anyway…

Run these commands the project root directory:


```sh
echo "optional-packages: vendor/**/*.cabal" > cabal.project.local
mkdir vendor
git clone --single-branch --branch add-monad-fail-inst git@github.com:vyorkin/telegram-bot-simple.git vendor/
make all
```
