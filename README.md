# Hakyll CMS

## Notice

This project is under heavy development, not functional and should not be used in production under any circumstances!

---

This is a CMS frontend for the Haskell based static site generator [Hakyll](https://jaspervdj.be/hakyll/). Its aim is to make writing sites easier by providing a web interface that manages the content of your Hakyll installation.

## Dependencies

Currently this project needs some dependencies to be globally installed:

* [Stack](https://docs.haskellstack.org/en/stable/README/)
* [Happy](https://www.haskell.org/happy/)
* [Hsc2Hs](https://hackage.haskell.org/package/hsc2hs)

For the deployment script [Closure Compiler](https://developers.google.com/closure/compiler/) is also needed

## Tentative Tech Stack

The project is currently in the research and preliminary development stage.
Parts of the tech stack decisions are fairly finalized and will be listed here:

### Client

| Type | Technology |
|------|------------|
| Text Editor | [SimpleMDE](https://simplemde.com) |
| UI Style | [Semantic UI](https://semantic-ui.com) via [reflex-dom-semui](https://github.com/reflex-frp/reflex-dom-semui) |

### Server

| Type | Technology |
|------|------------|
| API Layer | [Servant](http://haskell-servant.readthedocs.io/en/stable/) |
