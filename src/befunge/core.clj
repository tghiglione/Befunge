(ns befunge.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

;; Dimensiones de la grilla
(def grid-columnas 80)
(def grid-filas 25)

;; Estado inicial del intérprete
(def estado-inicial
  {:pc {:x 0 :y 0 :dir :right}
   :stack '()
   :grid nil
   :string-mode false})

;; Función para leer el programa desde un archivo
(defn read-program
  [file-path]
  (with-open [reader (io/reader file-path)]
    (doall (line-seq reader))))

;; Función para crear y cargar la grilla
(defn crear-grilla
  [lines]
  (vec
    (map (fn [linea]
           (vec (concat
                  (take grid-columnas linea)
                  (repeat (- grid-columnas (count linea)) \space))))
         (concat
           (take grid-filas lines)
           (repeat (- grid-filas (count lines)) "")))))

;; Función -main
(defn -main
  [& args]
  (let [file-path (first args)]
    (if file-path
      (println "Ruta del archivo Befunge-93:" file-path)
      (println "Por favor, proporciona la ruta del archivo Befunge-93 a ejecutar."))))