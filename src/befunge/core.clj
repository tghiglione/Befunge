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
        a (or (first stack) 0)
        b (nth stack 1 0)
        rest (drop 2 stack)
        resultado (op b a)]
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
        b (nth stack 1 0)
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

;; Funcion para leer un entero y apilar
(defn input-int
  [estado]
  (print "Ingrese un número entero: ")
  (flush)
  (let [input (read-line)
        valor (try
                (Integer/parseInt input)
                (catch Exception _ 0))]
    (update estado :stack conj valor)))

;; Funcion para leer un caracter y apilar el ASCII
(defn input-char
  [estado]
  (print "Ingrese un carácter: ")
  (flush)
  (let [input (read)
        valor (int input)]
    (update estado :stack conj valor)))

;; Funcion para desapilar un valor y cambiar la direccion del PC horizontalmente
(defn horizontal-if
  [estado]
  (let [stack (:stack estado)
        a (or (first stack) 0)
        rest (rest stack)
        dir (if (zero? a) :right :left)]
    (-> estado
        (assoc :stack rest)
        (assoc-in [:pc :dir] dir))))

;; Funcion para desapilar un valor y cambiar la direccion del PC verticalmente
(defn vertical-if
  [estado]
  (let [stack (:stack estado)
        a (or (first stack) 0)
        rest (rest stack)
        dir (if (zero? a) :down :up)]                       ;
    (-> estado
        (assoc :stack rest)
        (assoc-in [:pc :dir] dir))))

;; Función que desapila y, x, obtiene el valor en esa posición de la grilla y lo apila
(defn command-get
  [estado]
  (let [stack (:stack estado)
        y (or (first stack) 0)
        x (nth stack 1 0)
        rest (drop 2 stack)
        grid (:grid estado)
        within-range? (and (<= 0 x (dec grid-columnas))
                           (<= 0 y (dec grid-filas)))
        value (if within-range?
                (int (get-in grid [y x] \0))
                0)]
    (assoc estado :stack (conj rest value))))

;; Funcion que desapila x,y, valor y coloca el caracter en la grilla
(defn command-put
  [estado]
  (let [stack (:stack estado)
        y (or (first stack) 0)
        x (nth stack 1 0)
        v (nth stack 2 0)
        rest (drop 3 stack)
        grid (:grid estado)
        within-range? (and (<= 0 x (dec grid-columnas))
                           (<= 0 y (dec grid-filas)))]
    (if within-range?
      (let [char (char v)
            new-grid (assoc-in grid [y x] char)]
        (-> estado
            (assoc :stack rest)
            (assoc :grid new-grid)))
      (assoc estado :stack rest))))

;; Función principal para interpretar comandos
(defn interpretador-comando
  [estado comando]
  (let [{:keys [stack pc grid]} estado]
    (cond
      ;; Números del 0 al 9
      (Character/isDigit comando)
      (update estado :stack conj (Character/digit comando 10))

      (= comando \>) (assoc-in estado [:pc :dir] :right)
      (= comando \<) (assoc-in estado [:pc :dir] :left)
      (= comando \^) (assoc-in estado [:pc :dir] :up)
      (= comando \v) (assoc-in estado [:pc :dir] :down)

      (= comando \#) (update estado :pc mover-pc)

      (= comando \") (assoc estado :string-mode true)

      (= comando \+) (operacion-binaria estado +)
      (= comando \-) (operacion-binaria estado -)
      (= comando \*) (operacion-binaria estado *)
      (= comando \/) (operacion-binaria estado (fn [b a] (if (zero? a) 0 (quot b a))))
      (= comando \%) (operacion-binaria estado (fn [b a] (if (zero? a) 0 (mod b a))))

      (= comando \!) (operacion-unaria estado (fn [a] (if (zero? a) 1 0)))
      (= comando \`) (operacion-binaria estado (fn [b a] (if (> b a) 1 0)))

      (= comando \.) (imprimir-entero estado)
      (= comando \,) (imprimir-char estado)
      (= comando \&) (input-int estado)
      (= comando \~) (input-char estado)

      (= comando \_) (horizontal-if estado)
      (= comando \|) (vertical-if estado)
      (= comando \?) (assoc-in estado [:pc :dir] (rand-nth [:up :down :left :right]))

      (= comando \:) (duplicar-top estado)
      (= comando \\) (intercambiar-top estado)
      (= comando \$) (assoc estado :stack (rest (:stack estado)))

      (= comando \g) (command-get estado)
      (= comando \p) (command-put estado)

      (= comando \@) (assoc estado :finished true)

      (= comando \space) estado

      :else
      (do
        (println (str "Comando no reconocido o no implementado: " comando))
        estado))))

;; Función principal de ejecución
(defn ejecutar-programa
  [estado]
  (if (:finished estado)
    (println "\nEjecución finalizada.")
    (let [{:keys [pc grid stack string-mode]} estado
          {:keys [x y dir]} pc
          current-cell (get-in grid [y x])]
      (if string-mode
        ;; Modo cadena
        (if (= current-cell \")
          (let [new-state (assoc estado :string-mode false)
                next-pc (mover-pc (:pc new-state))
                updated-state (assoc new-state :pc next-pc)]
            (recur updated-state))
          (let [new-state (update estado :stack conj (int current-cell))
                next-pc (mover-pc (:pc new-state))
                updated-state (assoc new-state :pc next-pc)]
            (recur updated-state)))
        ;; Modo normal
        (let [new-state (interpretador-comando estado current-cell)
              next-pc (mover-pc (:pc new-state))
              updated-state (assoc new-state :pc next-pc)]
          (recur updated-state))))))

(defn -main
  [& args]
  (let [file-path (first args)]
    (if file-path
      (let [program-lines (read-program file-path)
            grid (crear-grilla program-lines)
            estado-inicial (assoc estado-inicial :grid grid)]
        ;; Ejecutar el programa
        (ejecutar-programa estado-inicial))
      (println "Por favor, proporciona la ruta del archivo Befunge-93 a ejecutar."))))