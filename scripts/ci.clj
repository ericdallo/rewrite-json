(ns ci
  (:require
   [babashka.tasks :refer [shell]]
   [clojure.string :as string]))

(defn ^:private replace-in-file [file regex content]
  (as-> (slurp file) $
    (string/replace $ regex content)
    (spit file $)))

(defn ^:private update-readme-version [tag]
  (replace-in-file "README.md"
                   #"dev\.ericdallo/rewrite-json \{:mvn/version \"[^\"]+\"\}"
                   (format "dev.ericdallo/rewrite-json {:mvn/version \"%s\"}" tag))
  (replace-in-file "README.md"
                   #"\[dev\.ericdallo/rewrite-json \"[^\"]+\"\]"
                   (format "[dev.ericdallo/rewrite-json \"%s\"]" tag)))

(defn ^:private add-changelog-entry [tag comment]
  (replace-in-file "CHANGELOG.md"
                   #"## Unreleased"
                   (if comment
                     (format "## Unreleased\n\n## %s\n\n- %s" tag comment)
                     (format "## Unreleased\n\n## %s" tag))))

(defn jar []
  (shell "clojure -T:build jar"))

(defn install []
  (shell "clojure -T:build install"))

(defn tag [& [tag]]
  (shell "git fetch origin")
  (shell "git pull origin HEAD")
  (spit "resources/REWRITE_JSON_VERSION" tag)
  (add-changelog-entry tag nil)
  (update-readme-version tag)
  (jar)
  (shell "git add pom.xml resources/REWRITE_JSON_VERSION CHANGELOG.md README.md")
  (shell (format "git commit -m \"Release: %s\"" tag))
  (shell (str "git tag " tag))
  (shell "git push origin HEAD")
  (shell "git push origin --tags"))

(defn deploy-clojars []
  (shell "clojure -T:build deploy-clojars"))

(defn tests []
  (shell "clojure -M:test"))
