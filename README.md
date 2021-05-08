# bubbles

Generative art using [clojurescript](https://clojurescript.org/) and [reagent](https://reagent-project.github.io/) mixed with hand-rolled [genetic algorithms](https://towardsdatascience.com/introduction-to-genetic-algorithms-including-example-code-e396e98d8bf3).

The goal of this project was to test whether generative art could be created by creative dummies like myself. If you lack artistic sense like I do, you can't come up with good mixes of colors, spatial distributions of elements, or distributions of size of elements. Bubbles does that for you. Just pick your two favourite individuals, and bubbles will create a new population based on mutations of those two. You can do this forever, or until the model has converged.

Check the demo at https://jgpaiva.github.io/bubbles

## Local development

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

## License

Copyright © 2020 João Paiva

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
