# Workshop: Haskell

## Linksammlung

### Tools
- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [Intero](https://haskell-lang.org/intero)
- [VS.Code & Haskelly](https://github.com/haskelly-dev/Haskelly)


### Hilfreiches
- S. Diehl: [Numeric Tower](http://dev.stephendiehl.com/hask/#numeric-tower)
- J. Bailey: [Haskell CheatSheet](http://blog.codeslower.com/static/CheatSheet.pdf)

### Hintergrund
- S. Diehl: [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/)
- Wikipedia: [Non-strict evaluation](https://en.wikipedia.org/wiki/Evaluation_strategy#Non-strict_evaluation)
- Hudak, Hughes, Jones, Wadler: [A History of Haskell: Being Lazy With Class](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/history.pdf)
- Haskell-Wiki: [Lazy vs. non-strict](https://wiki.haskell.org/Lazy_vs._non-strict)
- H. Apfelmus:  [Incomplete Guide to Lazy Evaluation](https://hackhands.com/guide-lazy-evaluation-haskell/)
- [Overloading in Haskell](http://www.cse.chalmers.se/edu/year/2016/course/TDA452_Functional_Programming/lectures/OverloadingAndTypeClasses.html)

## Installation

### Stack
Ich empfehle Haskell nur über das [**Stack Tool**](https://docs.haskellstack.org/en/stable/README/) zu installieren
(also nicht die Platform oder den GHC direkt).

In jeden Fall solltet ihr nach der Installation mit

```
stack --version
stack update
stack upgrade
```

prüfen (die Version jetzt gerade ist 1.4.0) und eventuell *Upgraden*
(falls nötig).

#### Windows
Siehe [Install and Upgrade Windows](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows)

Es gibt Installer für

- [64-bit](https://www.stackage.org/stack/windows-x86_64)
- [32-bit](https://www.stackage.org/stack/windows-i386)

Das Installationsverzeichnis solltet Ihr in eure `PATH` Umgebungsvariable 
aufnehmen.

#### MacOS
Zum Mac kann ich leider nicht viel sagen, aber eine
Anleitung findet ihr [hier](https://docs.haskellstack.org/en/stable/install_and_upgrade/#macos)

#### Linux
Für **Ubuntu** und co. gibt es eine `deb` Quelle: `deb http://download.fpcomplete.com/ubuntu xenial main`

Ansonsten bitte unter [How to install](https://docs.haskellstack.org/en/stable/README/#how-to-install)
schauen.

### Intero
Intero ist meiner Meinung nach aktuelle die beste Möglichkeit guten
Editor-Support für Haskell zu bekommen.

Eine Installationsanleitung findet ihr [hier](https://haskell-lang.org/intero).

Übrigens: dort steht viel über Emacs - das unten vorgestellte *VS.code* Plugin
nutzt aber auch Intero.

### Editorsupport

#### VS Code
Ich benutze [Haskelly](https://marketplace.visualstudio.com/items?itemName=UCL.haskelly)

Installationsanleitung findet ihr unter dem Link.

Im Prinzip läuft es aber darauf hinaus *Stack* (siehe oben) zu installieren
und dann *Intero* und *Quickcheck* per:

    stack install intero QuickCheck

nachzuziehen.

Auf Linux/MacOS könnt ihr noch

    stack install stack-run
    
installieren, dass den praktischen `stack run` befehl beinhaltet,
das geht aber glaube ich auf Windows nicht.


#### Emacs
Wird im Prinzip auf der [Intero Seite](https://haskell-lang.org/intero) erklärt.
Hier aber noch die relevanten Teile aus meiner `.emacs` Konfig (verwendet 
offensichtlich noch eine Menge andere Emacs packages - also nur wenn ihr euch
das zutraut, bzw. wissen wollt, wie das bei mir aussieht):

```commonlisp
;; ----------------------------------------------------------------------
;; HASKELL
(require 'haskell-mode)
(require 'intero)

(add-hook 'haskell-mode-hook 'intero-mode)
(setq flycheck-check-syntax-automatically '(save new-line))
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

;; Zusätzliche Modi
(add-hook 'haskell-mode-hook 'linum-mode)
(add-hook 'haskell-mode-hook 'column-enforce-mode)
(add-hook 'haskell-mode-hook 'projectile-mode)
(add-hook 'literate-haskell-mode-hook 'column-enforce-mode)

;; Key-Bindings
(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-t") 'intero-type-at)
     (define-key haskell-mode-map (kbd "C-c C-i") 'intero-info)
     ))
```

#### andere Editoren
sorry aber Da müsst ihr selbst suchen - ich nutze nur diese beiden :(
