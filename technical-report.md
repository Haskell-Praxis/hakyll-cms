# Hakyll-CMS Technical Report


<!--

"wenn abnehmer mit produkt nicht einverstanden ist, von gutachter/anwält\*in gelesen"

* was war aufgabe? (~ pflichtenheft)
* welche grundlagen / ressourcen / state-of-the-art? (e.g. vorhandene libs)
* was getan?
 * entscheidungen nachvollziehbar dokumentieren
 * für lva ok: "wir hatten nicht genug zeit den fall weiter zu verfolgen"

kurz / knapp und präzise. synonyme vermeiden. möglichst wenig juristische angriffsfläche.

Additional: write in a way that it can be used as docs on gh

-->


## Goal

An application for writing and managing posts which are consumed by an arbitrary Static Site Generator^[a program that generates static (html and otherwise) content to be served by a simple web server] (e.g. Hakyll). The application should be web-based, have a rich text editor^[showing an approximation of the final document in the editor and allowing shortcuts to change formatting] for posts, save the content as markdown files and be written in Haskell.

<!--
Additional features required for a production ready minimum viable product would include:

* saving the posts to and loading them from the hard disk
* 


Einfach zu serven/deployen
Verbrauchen wenig Ressourcen
Beliebiger Text-Editor
Posten... 
Braucht technische Skills
Aufwändig

Git (oder SSH-Access)
Text-Datei anlegen
Markdown
Generator in Shell ausführen

Zugriff via Browser-URL
Recognition over Recall bez Markdown-Syntax
Veröffentlichen via Button-Click
Informative Übersicht über Artikel
Aber auch: Vorteile von SSG

-->

## State-Of-The-Art and Tooling Decisions

### Text-Editor Library

<!-- https://github.com/Haskell-Praxis/hakyll-cms/issues/1 -->

We tried out demos of all currently available javascript-based text-editors we found and compared them by:

* Uncompressed Size
* Whether they support rich-editing or exposed the markdown to users
* Whether they can directly export the markdown we needed for the posts or HTML we'd need to convert first
* Whether or not they offer a toolbar that allows recognizing markup options instead of needing to recall them.

The comparison-table below is sorted by uncompressed size. Bootstrap is about 28k uncompressed, jQuery about 91k. Sizes have been determined by looking at the network tab on the demo pages (subtracting any content, like images, in those demos).

| Editor | Size | Editing | Exports | Toolbar | Notes |
| --- | --- | --- | --- | --- | --- |
| [Medium.js](http://jakiestfu.github.io/Medium.js/docs/) | 18k | rich | html | JS-API | |
| [Trumbowyg](http://alex-d.github.io/Trumbowyg/) | 21k + jQuery | rich | html | at top | |
| [pen](http://sofish.github.io/pen/) | 32k | rich | md | mouse-over ||
| [EpicEditor](http://stacks.math.columbia.edu/js/EpicEditor/) | 34k | md  with preview | md | none | |
| [bootstrap-markdown](http://www.codingdrama.com/bootstrap-markdown/) | 49k + Bootstrap | md with preview | md | at top | |
| [CLEditor](http://premiumsoftware.net/cleditor/) | 9k + jQuery | rich | html | at top | win95 style |
| [bootstrap-wysiwyg](http://mindmup.github.io/bootstrap-wysiwyg/) | 5k + jQuery + Bootstrap | rich | html export | at top ||
| [Lepture's Editor](http://lab.lepture.com/editor/) | 247k | md with preview | md | at top ||
| [Vue.js](https://vuejs.org/v2/examples/) | 273k | md with preview | md | none ||
| [SimpleMDE](https://simplemde.com/) | 280k | md with preview | md | at top ||
| [Grafikart/JS-Markdown-Editor](https://github.com/Grafikart/JS-Markdown-Editor) | 189k + jQuery + Bootstrap | md  with preview | md | at top ||
| [medium-editor](https://github.com/daviferreira/medium-editor) | 311k | rich | html | mouse-over ||
| [bootstrap-wysihtml5](http://jhollingworth.github.com/bootstrap-wysihtml5/) | 325k | rich |  html | at top ||
| [TinyMCE](https://www.tinymce.com/) | 405k | rich | html | at top | many markup options |
| [ACE](https://ace.c9.io/build/kitchen-sink.html) | 645k | md amongst others | md if md is written | none | code editor with syntax highlighting. pre-existing reflex-wrapper(!) |
| [Hallo.js](http://hallojs.org/demo/markdown/) | 106k + 566k dependencies | rich + md | md | at top ||
| [CKEditor + Markdown Plugin](http://ckeditor.com/addon/markdown) | ~820k | rich + md | md | at top | many markup options, [configuration-tool](http://ckeditor.com/builder), uses iframes |
| [jbt/markdown-editor](https://jbt.github.io/markdown-editor/) | 823k with various dependencies | md with preview | md | none | default style unusable on small screens |
| [Markdown-It](https://markdown-it.github.io/) | 250k + 850k dependencies | md  with preview | md | none ||
| [Substance](http://substance.io/) | 1.14M | rich | html | at top | |
| [Stackedit](https://stackedit.io/) | 1.48M | md with preview | md | at top | requires nodejs server |
| [Woofmark](https://bevacqua.github.io/woofmark/) | 1.4M | rich + md | md | at top | |
| [Pandao/Editor.md](https://pandao.github.io/editor.md/en.html) | ~1.7M with various dependencies | md with preview | md | at top | many markup options; [github-repo](https://github.com/pandao/editor.md)
| [Dillinger](http://dillinger.io/) | 1.8M | md with preview | md | none | various import/export options |
| [Markdown-Plus](http://mdp.tylingsoft.com/) | 6M with various dependencies | md  with preview | md | at top ||
| Aloha | | | | | discontinued |

Other comparisons:

* [This issue](https://github.com/Semantic-Org/Semantic-UI/issues/222) on Semantic-Org/Semantic-UI lists a few editors
* [Top 7: Best Markdown editors Javascript and jQuery plugins](http://ourcodeworld.com/articles/read/359/top-7-best-markdown-editors-javascript-and-jquery-plugins) (2017/01)
* [Coding 10 Awesome JavaScript WYSIWYG & Markdown Editors](http://www.developersfeed.com/awesome-javascript-wysiwyg-markdown-editors/) (2016)

#### Decision

Going through the editors by size, [SimpleMDE](https://simplemde.com/) seems to be the right choice for our project. It is the smallest library that satisfies: 

* Good usability
  * A toolbar for markup-options ("recognition over recall"), as opposed to Epic and Vue.js.
  * Rich-looking code-styling (e.g. headings are larger), as opposed to bootstrap-md
  * A split-screen preview of the resulting HTML
  * Relatively modern looking, clean styling / visual design, as opposed to CLE
* Markdown-editing (no conversion to and from HTML necessary as with Pen)
* A mature API that gives us refined change events and that distinguishes it from the smaller [Pen](http://sofish.github.io/pen/) and [Lepture's Editor](http://lab.lepture.com/editor/))

### CSS-Library

There are wide-variety of different CSS-libraries. To name the more widely used:

* [Bootstrap](http://getbootstrap.com/)
* [Semantic-UI](http://semantic-ui.com/)
* [Tachyons](http://tachyons.io/)
* ...

**Decision:** For Semantic-UI, there exists a Reflex-based wrapper in the form of [reflex-frp-semui](https://github.com/reflex-frp/reflex-dom-semui) that provides everything we need for the prototype.

### Web Interface Structure

One of the goals of the application was that most parts should be written in Haskell. This did not leave many options for the architecture.

| Solution | Description | Backend Option |
| -------- | -------- | ------- |
| HTML/JavaScript | Using Static HTML supplemented with JavaScript for the dynamic parts of the application | HTML Renderer |
| Elm | A Haskell-like client language which compiles to JavaScript | REST Server |
| Fay | An older Haskell to JavaScript compiler which only supports a small subset of Haskell | REST Server |
| Haste | A Haskell Backend/Frontend combination with a predefined interface between the two | Haste |
| GHCJS | A full Haskell-to-JS compiler with support for JavaScript FFI | REST Server |

Some more options are available in the Haskell Wiki^[[https://wiki.haskell.org/The_JavaScript_Problem](https://wiki.haskell.org/The_JavaScript_Problem)]. But almost all of them were unmaintained at time of writing.

Since the application was assumed to have a very dynamic frontend, rendering static HTML and supplementing it with JavaScript was quickly abandoned as a solution.

Using Elm in the frontend did also not meet the criterium of using mostly Haskell as a programming language. This would have introduced a separate codebase to maintain as well as a boundary layer problem between Haskell and Elm.

Of the Haskell-to-JS compilers, only GHCJS was maintained and stable enough for actual use. Its binary output size is very big - around 300kb overhead pre-gzip (we assume mostly due to support for things like lazy-evaluation). This was not a big problem for our use-case however, since we did not expect many new users to visit the application. Rather, users would use the application more often, making caching very efficient and reducing the overall impact of a big JavaScript-bundle.

**Decision:** We decided to use GHCJS on the client side because it provided us with a unified codebase.

#### Frontend

In addition to the frontend language, we needed a method to produce HTML and support interaction with the application. The most prominent GHCJS project to fill this gap was Reflex-DOM, an FRP implementation for web frontends.

Other solutions found - e.g. a wrapper around React - were generally not as well supported.

**Decision:** We opted for Reflex-DOM as the rendering solution for its rather big community and good maintenance, as well as its FRP paradigm.


#### Backend

Since the client side was decided to be an independent Haskell application, the backend would be a REST Server.
Since there is a best-practice solution for creating REST-APIs in Haskell, no extensive research was done before choosing a backend.

**Decision:** Servant and Servant-Server were chosen as the backend because they were the go-to solution for creating and serving REST-APIs in Haskell and provided a type-safe method of defining the API.


### Build Setup

At time of writing there were four major build setups for Haskell applications.

* Cabal
* Cabal New-Build
* Nix
* Stack

Cabal had been superseded by Cabal New-Build, although this transition was not complete at the time this project started.

Cabal New-Build looked promising, but had some major tooling flaws. For one, most of our major dependencies -- Reflex and Reflex-DOM -- were not actually in Hackage, and Cabal New-Build did not provide a standard way to fetch these packages from Git. Also, most code-sensing tools and IDEs were not compatible with Cabal New-Build, making development more tedious.

Nix was the build-tool of choice for the reflex-platform, but required an additional program to be installed on developers' systems, introduced a new programming language for configuration and added a lot of complexity to the build.

Finally Stack supported multiple compilers in its configuration and allowed pulling dependencies from Git. It was also the most widely used solution for developing Haskell.

**Decision:** We decided to use Stack as it provided the features we needed and was the industry-standard at the time.


<!-- cabal (newbuild) vs stack --> 

<!--
haskell-webapps vs reflex-platform vs custom
Servant
Text-Editor Vergleich (gh-issue)
cabal new-build vs stack vs nix
JSaddle und GHCJS

Probleme:
GHCJS wird von Stack nicht sehr gut supported
Cabal-Newbuild wird von devtools nicht richtig unterstützt
Ghc-mod crashes...
GHCJS ist nicht mit allen dependencies kompatibel
Stacks versprechen zu portable builds nicht ganz richtig
Sehr viele stack files...


-->

## Development

### Wrapper for SimpleMDE

We implemted a minimal, reflex- and JSM-based wrapper for SimpleMDE that allows instantiating a fully styled editor with a custom, initial text-value and yields a `Dynamic t Text` that holds the current text-value of the editor and "notifies subscribers" of changes. 

We had to work around [a bug in Simple-MDE](https://github.com/nestor-qa/nestor/issues/98) that requires calling `simplemde.codemirror.refresh()` a second after it's initialisation, to make the initial text-value show.

From the general experience, I can recommend to start learning reflex and jsaddle/JSM using pre-wrapped widgets and elements, as writing a wrapper is more complicated, requires understanding and interfacing with several libraries, especially with less documented or more complicated functions of those. Also, I'd recommend using the `MonadWidget` catch-all type-constraint, as using a smaller / more selective constraint requires understanding and constructing a more complicated type-signature (`MonadWidget` hides a lot of those issues)

The hakyll-cms/simple-mde sub-project also contains a simple usage example.

Gabriel has started defining a data-type for the full set of SimpleMDE's configuration options. The structure is inspired by the work done on the [ACE-wrapper for reflex-dom](https://github.com/reflex-frp/reflex-dom-ace)



### Servant-API

<!-- 

issues / hoisting


autogen of handlers. client-side wrappers using `IO`. ...

-->

Servant provides a simple way to define APIs using Data Kinds^[[https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/type-level-literals.html](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/type-level-literals.html)]. Since now each API has an associated type, the endpoints are also typed, allowing extra checks about whether a server or client conforms to the specification. We were also able to automatically generate a Haskell client for our REST Api. There were a few pain points in this process, because the type of the client handlers is somewhat opaque, and they are "extracted" from the `client` call by pattern matching.

```haskell
getPostSummariesGeneric :<|> createPostGeneric :<|> postApi = client api

getPostGeneric id =
  let getPostGeneric' :<|> _ :<|> _ = postApi id
  in getPostGeneric'

updatePostGeneric id =
  let _ :<|> updatePostGeneric' :<|> _ = postApi id
  in updatePostGeneric'

deletePostGeneric id =
  let _ :<|> _ :<|> deletePostGeneric' = postApi id
  in deletePostGeneric'
```

Defining the API and handler on the server-side was straightforward for static content. Reading posts from and storing them to disk on the other hand required the addition of multiple monad transformers. Parsing strings into `Path`s required a `MonadThrow` monad for example. Since servants `Handler` wraps an `ExceptT ServantErro IO` the underlying monad needed to be changed. Servant provides a facility for hoisting to this end. A function can be defined to convert one monad transformer stack into another. For using a `Reader String` the function would look like this:

```haskell
let nt = generalizeNat . (runReaderTNat "hi") :: Reader String :~> Handler
```

Adding the monad transformers we need via this method proved to be difficult however, and due to time constraints the implementation of a file-backend was scrapped. Instead an in-memory backend was implemented via `STM`, lifting all `STM`-actions into `IO`

```haskell
didFail <- liftIO $ atomically $ do
        currentState <- readTVar tvar
        if member id currentState then
            return True
        else do
            writeTVar tvar (insert id post currentState)
            return False
```

This method removed a lot of the power for error-handling from Haskell's monad stacks and is only suitable for a rough prototype, but it provided the full backend functionality to our client interface.

### Overview and Edit-View

The client-app-prototype consists of three views:

* The overview (see screenshot), that shows post-teasers, post-titles and placeholder-images (that are to be replaced with proper images or removed for the MVP)
* An edit-view (see screenshot), that allows editing posts
* A post-creation view, that allows creating new posts and GUI-wise only differs to the edit-view by the title.

![Overview](https://i.imgur.com/BBstgut.png)

![Edit-view](https://i.imgur.com/hyfy4HK.png)

To allow navigating between these views, we used the basic routing-utilities from `reflex-frp/reflex-dom-contrib`. Even though these are experimental, these utilities -- and the more powerful but also more complicated `MonadRouted` that builds upon these -- are the best solution available in Reflex at the time of this writing. Routing happens via URL-fragments. This allows us to use plain links for navigation (with no additional wiring required). Also, it avoided extra configuration effort with jsaddle-warp (or a custom HTTP-server) to redirect HTTP-requests to always serve `index.html`, regardless of which route was requested (as described in [this issue](https://github.com/ghcjs/jsaddle/issues/34)). E.g. `http://localhost:8081/#/edit/getting-started_2017-01-10` automatically loads `index.html`.

For the form, we implemented a small widget, that groups a text-field with it's label and that should be part of `reflex-frp/reflex-dom-semui`, as it's a Semantic-UI staple.


## Future Work

To make this very crude prototype into something more of a minimum viable product that can be properly released the following tasks need to be done:

* [GHCJS compiler issues](https://github.com/Haskell-Praxis/hakyll-cms/issues/12) with some use of template-haskell.
* Saving posts as files with correct YAML-frontmatter
* Show rendered HTML -- instead of markdown -- in the content-teaser in the overview (as described in [this issue](https://github.com/Haskell-Praxis/hakyll-cms/issues/14)).
* [Error recovery](https://github.com/Haskell-Praxis/hakyll-cms/issues/11) for problems users might encounter in the wild
* [Elipses](https://github.com/Haskell-Praxis/hakyll-cms/issues/10) at the end of post-summaries, that don't already include the entire post.
* Deleting posts.
* Fixing the initialization bug in SimpleMDE -- via a pull-request to their repository.
* Access-control
* Optional: Interactions with Git, as this gives us versioning and allows interactions with other systems (e.g. people using their own text-editor on their own machine)

As an optional side-project we could/will finish wrapping the entirety of SimpleMDE (i.e. all configuration options and events it emits) and publishing that library as contribution to the Reflex-/GHCJS-community.

Also we found several places where we could/will contribute to reflex-dom-contrib and reflex-dom-semui.
