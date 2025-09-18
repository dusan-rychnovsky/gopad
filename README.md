[![Reliability Rating](https://sonarcloud.io/api/project_badges/measure?project=dusan-rychnovsky_gopad&metric=reliability_rating)](https://sonarcloud.io/summary/new_code?id=dusan-rychnovsky_gopad)
[![Maintainability Rating](https://sonarcloud.io/api/project_badges/measure?project=dusan-rychnovsky_gopad&metric=sqale_rating)](https://sonarcloud.io/summary/new_code?id=dusan-rychnovsky_gopad)
[![Security Rating](https://sonarcloud.io/api/project_badges/measure?project=dusan-rychnovsky_gopad&metric=security_rating)](https://sonarcloud.io/summary/new_code?id=dusan-rychnovsky_gopad)
[![Vulnerabilities](https://sonarcloud.io/api/project_badges/measure?project=dusan-rychnovsky_gopad&metric=vulnerabilities)](https://sonarcloud.io/summary/new_code?id=dusan-rychnovsky_gopad)
[![Bugs](https://sonarcloud.io/api/project_badges/measure?project=dusan-rychnovsky_gopad&metric=bugs)](https://sonarcloud.io/summary/new_code?id=dusan-rychnovsky_gopad)  
[www.dusanrychnovsky.cz/gopad](https://dusanrychnovsky.cz/gopad)

# GoPad

An `Elm`-based single-page web application which allows you to capture a game of `Go` for future reference.

Ideally open the page on your tablet and place it next to your goban. As you play, click (tap) on the board in the app to place your stones. When the game is finished, click `Save Game` to export the game in `SGF` format (a widely supported standard for storing records of board games like Go). You can upload the saved file to platforms such as `OGS`, or analyze it with tools like `KataGo`.

Your browser's `Local Storage` is used to automatically preserve application state, so you won’t lose game progress if you accidentally close or refresh the browser, or experience a crash.

An example screenshot:
<p align="center">
  <img width="412" height="447" alt="image" src="https://github.com/user-attachments/assets/7d3a82e3-5132-4b9c-a0ad-be9853a4fb41" />
</p>

## How To

### Build

Install `Elm` by following this guide: [https://guide.elm-lang.org/install/](https://guide.elm-lang.org/install/elm.html) and execute these commands in your terminal:
```
$ git clone https://github.com/dusan-rychnovsky/gopad.git
$ cd gopad
$ elm make src/Main.elm --output=elm.js
```

Then open `index.html` in your browser. The full application requires also `elm.js` and `public` folder, which are referenced from `index.html`.

### Make Changes

After you make any changes to the source code, make sure to run the following commands to test and properly format your code:

```
$ cd gopad
$ elm-format src/ tests/ --yes
$ elm-test
```
