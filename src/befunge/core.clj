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

;; Función para mover el PC
(defn mover-pc
  [pc]
  (let [{:keys [x y dir]} pc
        nuevo-pc (case dir
                 :right {:x (mod (inc x) grid-columnas) :y y :dir dir}
                 :left  {:x (mod (dec x) grid-columnas) :y y :dir dir}
                 :down  {:x x :y (mod (inc y) grid-filas) :dir dir}
                 :up    {:x x :y (mod (dec y) grid-filas) :dir dir})]
    nuevo-pc))

;; Función auxiliar para aplicar operaciones binarias en la pila
(defn operacion-binaria
  [estado op]
  (let [stack (:stack estado)
        a (or (first stack) 0)                              ;;guardo en 'a' el valor superior de la pila. si no hay guardo 0
        b (or (nth stack 1) 0)                              ;;guardo en 'b' el segundo valor superior de la pila. si no hay guardo 0
        rest (drop 2 stack)                                 ;;guardo en rest el resto de la pila menos los ultimos dos elementos
        resultado (op b a)]                                 ;realizo la operacion y lo guardo en resultado
    (assoc estado :stack (conj rest resultado))))

;; Función auxiliar para aplicar operaciones unarias en la pila
(defn operacion-unaria
  [estado op]
  (let [stack (:stack estado)
        a (or (first stack) 0)
        rest (rest stack)
        resultado (op a)]
    (assoc estado :stack (conj rest resultado))))

;; Funcion para duplicar el elemento superior de la pila
(defn duplicar-top
  [estado]
  (let [stack (:stack estado)
        a (or (first stack) 0)]
    (update estado :stack conj a)))

;; Funcion para intercambiar los dos elementos superiores de la pila
(defn intercambiar-top
  [estado]
  (let [stack (:stack estado)
        a (or (first stack) 0)
        b (or (nth stack 1) 0)
        rest (drop 2 stack)]
    (assoc estado :stack (conj rest a b))))

;; Funcion para desapilar e imprimir como entero
(defn imprimir-entero
  [estado]
  (let [stack (:stack estado)
        a (or (first stack) 0)
        rest (rest stack)]
    (print (str a " "))
    (assoc estado :stack rest)))

;; Funcion para desapilar e imprimir como ASCII
(defn imprimir-char
  [estado]
  (let [stack (:stack estado)
        a (or (first stack) 0)
        rest (rest stack)]
    (print (char a))
    (assoc estado :stack rest)))

;; Función -main
(defn -main
  [& args]
  (let [file-path (first args)]
    (if file-path
      (println "Ruta del archivo Befunge-93:" file-path)
      (println "Por favor, proporciona la ruta del archivo Befunge-93 a ejecutar."))))