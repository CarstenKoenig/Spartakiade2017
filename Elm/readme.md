# Workshop: ELM

## Skript
Mein Skript aus dem Workshop findet ihr [hier](./Workshop.md)

## Beispiel-Apps

## Taschenrechner
[hier](./Calc)

## Uhr
[hier](./Uhr)

## Tests
Testframework / Benutzung [hier](./Tests)

Ihr braucht dazu noch das [`elm-test` Tool](http://package.elm-lang.org/packages/elm-community/elm-test/latest)

Einfach im Verzeichnis

    elm test
	
aufrufen.

## Linksammlung

### Elm
- [Homepage](http://elm-lang.org/)
- [Packages Docs](http://package.elm-lang.org/)
- [Syntax](http://elm-lang.org/docs/syntax)
- [Javascript -> Elm](http://elm-lang.org/docs/from-javascript)
- [Elm Bootstrap](http://elm-bootstrap.info/)

### nützliche Docs
- [Elm Core](http://package.elm-lang.org/packages/elm-lang/core/latest/)
- [Elm Html](http://package.elm-lang.org/packages/elm-lang/html/latest/)
- [Elm Bootstrap Docs](http://package.elm-lang.org/packages/rundis/elm-bootstrap/latest)

### Hintergrund
- [Elm Records](http://elm-lang.org/docs/records)
- [the Elm Architecture](https://guide.elm-lang.org/architecture/)

### Tools
- [Installation](https://guide.elm-lang.org/install.html)
- [elm-format](https://github.com/avh4/elm-format#installation)
- [elm-oracle](https://github.com/ElmCast/elm-oracle)
- [vscode-elm](https://github.com/sbrink/vscode-elm)

## Installation
### Elm
Elm selbst kann unter:

- **Windows** entweder als [Installer](http://install.elm-lang.org/Elm-Platform-0.18.exe) oder über [NPM](https://www.npmjs.com/package/elm) installiert werden.
- **Mac** ebenfalls als [Installer](http://install.elm-lang.org/Elm-Platform-0.18.pkg) oder [NPM](https://www.npmjs.com/package/elm)
- **Linux** über [NPM](https://www.npmjs.com/package/elm)

installiert werden.

Persönlich bevorzuge ich den Weg über **NPM** weil das überall geht:

```
npm install -g elm
```

bzw.

```
sudo install -g elm
```

### Elm Format
*Elm Format* ist sehr nützlich (automatische Quellcodeformatierung) und ich empfehle es mit zu installieren, auch wenn es am Anfang etwas
gewöhnungsbedürftig ist.

*Elm Format* kann man am einfachsten installieren, indem man die Binary irgendwo in seinen Pfad ablegt.

Die Binaries findet man für:

- **Windows** [hier](http://install.elm-lang.org/Elm-Platform-0.18.pkg)
- **Mac** [hier](https://github.com/avh4/elm-format/releases/download/0.5.2-alpha/elm-format-0.18-0.5.2-alpha-mac-x64.tgz)
- **Linux** [hier](https://github.com/avh4/elm-format/releases/download/0.5.2-alpha/elm-format-0.18-0.5.2-alpha-linux-x64.tgz)

### Elm Oracle
Manche Editor-Plugins (*elm-vim*, *atom-elm*, *elm-mode* (Emacs), *Elm.tmLanguage* (Sublime), *elm-light* (LightTable)) benötigen *Elm Oracle*

Die Installation läuft über **NPM**:

```
npm install -g elm-oracle
```

bzw.

```
sudo npm install -g elm-oracle
```

### Editorsupport
#### VS.code
siehe [vscode-elm](https://marketplace.visualstudio.com/items?itemName=sbrink.elm)
für *elm-format* kann noch [VS.code elm-format](https://marketplace.visualstudio.com/items?itemName=abadi199.elm-format) installiert werden

#### Emacs
ist in [**MELPA** (`elm-mode`)](https://github.com/jcollard/elm-mode) verfügbar, *elm-oracle* und *elm-format* sollten installiert sein.

hier ist der relevante Teil meiner Emacs-Config:

```commonlisp
;; -----------------------------------------------------------------------------
;; ELM
(require 'elm-mode)
(setq elm-format-on-save t)
(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-hook 'elm-mode-hook 'linum-mode)
(add-hook 'elm-mode-hook 'flycheck-mode)

;; Company Backend
(add-to-list 'company-backends 'company-elm)

;; Flycheck
(require 'flycheck-elm)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
``` 

außerdem habe ich folgendes in `customize-mode` (bzw. `.gnu-emacs-custom`), damit `elm-format` beim Speichern benutzt wird:

```commonlisp
 '(elm-format-on-save t)
```


### andere
siehe "Configure Your Editor" [hier](https://guide.elm-lang.org/install.html)


## Starter
Um schnell einen Startpunkt für eine eigene Anwendung zu haben, findest
Du im Verzeichnis `./Starter/Demo` bzw. `./Starter/Bootstrap` zwei
Startpunkte für eigene Projekte:

- die [Elm Demo-App](./Starter/Demo) - einfacher Ausgangspunkt ohne `Cmd`/`Sub`
- [Bootstrap Starter-App](./Starter/Bootstrap) - inkl. Bootsrap mit `Cmd`/`Sub`
