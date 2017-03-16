# Countdown

## Link-Samlung
- [Haskell-Spock](https://www.stackage.org/haddock/lts-8.5/Spock-0.12.0.0/Web-Spock.html)
- [Haskell-Spock-Core](https://www.stackage.org/haddock/lts-8.5/Spock-core-0.12.0.0/Web-Spock-Core.html)
- [Elm-Bootstrap](http://elm-bootstrap.info/card)
- [Elm-Bootstrap API](http://package.elm-lang.org/packages/rundis/elm-bootstrap/latest)
- [Elm-Http](http://package.elm-lang.org/packages/elm-lang/http/1.0.0/Http)
- [Elm-Json](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode)

# Kompilieren

## Elm (optional)

    cd Countdown/client
    elm make main.elm --output ../static/client.js 
    
## Haskell - Spock Webapp

    cd Countdown
    stack build
    
# Ausführen

mit installierten `stack-run`

    cd Countdown
    stack run
    
sonst

    cd Countdown
    stack build
    stack exec Countdown
    

Jetzt sollte auf [localhost:8090](http://localhost:8090) die Application laufen.
