(ns app.core
  "This namespace contains your application and is the entrypoint for 'yarn start'."
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [app.verse :refer [verse]]))

(defn ^:dev/after-load render
  "Render the toplevel component for this app."
  []
  (rdom/render [verse] (.getElementById js/document "app")))

(defn ^:export main
  "Run application startup logic."
  []
  (render))
