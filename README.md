# FableConf Presentation

This is the project hosting my FableConf 2017 presentation files and projects

![Presentation sample](https://raw.githubusercontent.com/whitetigle/fableconf2017/master/images/sample.gif)

## How does it work?

It's a simple canvas based presentation.

- One canvas is used to draw the background animation
- whereas the texts are displayed on another canvas positionned on top of the first  (please check index.html to see css style)

Feel free to read and hack the source code. It's easy to read and well documented (hopefully).

Underlying tech stack:

* [Fable](http://fable.io/docs/getting-started.html) (obviously :wink:)
* [Elmish](https://fable-elmish.github.io/elmish/)
* [noisejs](https://github.com/josephg/noisejs)


# Build requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0 or higher
* [node.js](https://nodejs.org) 6.11 or higher
* A JS package manager: [yarn](https://yarnpkg.com) or [npm](http://npmjs.com/)

> npm comes bundled with node.js, but we recommend to use at least npm 5. If you have npm installed, you can upgrade it by running `npm install -g npm`.

Although is not a Fable requirement, on macOS and Linux you'll need [Mono](http://www.mono-project.com/) for other F# tooling like Paket or editor support.

## Building and running the app

> In the commands below, yarn is the tool of choice. If you want to use npm, just replace `yarn` by `npm` in the commands.

* Install JS dependencies: `yarn install`
* **Move to `src` folder**: `cd src`
* Install F# dependencies: `dotnet restore`
* Start Fable daemon and [Webpack](https://webpack.js.org/) dev server: `dotnet fable yarn-start`
* In your browser, open: http://localhost:8080/

### Paket

[Paket](https://fsprojects.github.io/Paket/) is the package manager used for F# dependencies. It doesn't need a global installation, the binary is included in the `.paket` folder. Other Paket related files are:

- **paket.dependencies**: contains all the dependencies in the repository.
- **paket.references**: there should be one such a file next to each `.fsproj` file.
- **paket.lock**: automatically generated, but should be committed to source control, [see why](https://fsprojects.github.io/Paket/faq.html#Why-should-I-commit-the-lock-file).
- **Nuget.Config**: prevents conflicts with Paket in machines with some Nuget configuration.

> Paket dependencies will be installed in the `packages` directory. See [Paket website](https://fsprojects.github.io/Paket/) for more info.

### yarn/npm

- **package.json**: contains the JS dependencies together with other info, like development scripts.
- **yarn.lock**: is the lock file created by yarn.
- **package-lock.json**: is the lock file understood by npm 5, if you use it instead of yarn.

> JS dependencies will be installed in `node_modules`. See [yarn](https://yarnpkg.com) and/or [npm](http://npmjs.com/) websites for more info.

### Webpack

[Webpack](https://webpack.js.org) is a bundler, which links different JS sources into a single file making deployment much easier. It also offers other features, like a static dev server that can automatically refresh the browser upon changes in your code or a minifier for production release. Fable interacts with Webpack through the `fable-loader`.

- **webpack.config.js**: is the configuration file for Webpack. It allows you to set many things: like the path of the bundle, the port for the development server or [Babel](https://babeljs.io/) options. See [Webpack website](https://webpack.js.org) for more info.
