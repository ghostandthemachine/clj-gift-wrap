(ns clj-gift-wrap.core
	(:use [clojure.reflect]
		  [clojure.pprint]
		  [inflections.core])
	(:import 
		(org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants TokenMakerFactory))
	(:require 
		  [clojure.string :as s]
		  [clojure.contrib.string :as cs]))

(def output (atom ""))

(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

(defn get-fn-type 
	[m]
	(cond
		(= "get" (str (s/join (take 3 (str (:name m))))))
	  :getter
	  (= "set" (str (s/join (take 3 (str (:name m))))))
	  :setter
	  (= (str (:return-type m)) "boolean")
	  :boolean))

(def alphabet ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"])

(defn parse-args
	[arguments]
	(let [num-args (count arguments)]
		(apply str (for [i (range num-args)] (str " " (nth alphabet i))))))

; (defn parse-args
; 	[arguments]
; 	(apply str (interleave " " arguments)))

(defn public? 
	[m]
	(and 
		(not (in? (:flags m) :private))
		(not (in? (:flags m) :protected))))

(defn static?
	[m]
	(in? (:flags m) :static))

(defn parse-fn-name
	[m]
	(let [type (get-fn-type m)
		  	name (cond
	  				(= type :setter)
	  				(str (s/join (drop 4 (hyphenize (str (:name m))))) "!")
	  				(= type :getter)
	  				(str (s/join (drop 4 (hyphenize (str (:name m))))))
	  				(= type :boolean)
						(str (hyphenize (str (:name m))) "?")
	  				:else
	  				(hyphenize (str (:name m))))]
 		(str name "\n")))
		
(defn public-method-to-string 
	[m]
	(str "(defn " (parse-fn-name m)
		"\"args " (str (:parameter-types m))"\"\n"
		"\"flags " (str (:flags m))"\"\n"
		"\"return-type " (str (:return-type m))"\"\n"
		"\t[obj" (parse-args (:parameter-types m)) "]\n"
		"\t(." (str (:name m)) " obj" (parse-args (:parameter-types m)) "))\n\n"))

(defn static-method-to-string 
	[m class-name]
	(if (public? m)
		(str "(defn " (parse-fn-name m)
		"\"args " (str (:parameter-types m))"\"\n"
		"\"flags " (str (:flags m))"\"\n"
		"\"return-type " (str (:return-type m))"\"\n"
		"\t[obj" (parse-args (:parameter-types m)) "]\n"
		"\t(" (:declaring-class m) "/" (str (:name m)) (parse-args (:parameter-types m)) "))\n\n")))

(def holder (atom nil))

(defn parse-method
	[method]
	(let [m method
		  class-name @holder]
		(cond 
			(and 
				(public? m)
				(not (static? m)))
			(public-method-to-string m)
			(static? m)
			(static-method-to-string m class-name)
			:else 
			"")))

(defn build-wrapper
	[obj]
	(swap! holder (fn [_] (s/join (drop 6 (str (type obj))))))
	(let [members (:members (reflect obj))
		  header (str "(ns " (cs/replace-char \. \_ (underscore @holder)) ".core)\n\n\n")
		  file-name (str (cs/replace-char \. \_ (underscore @holder)) ".clj")]
		(->> members
			(map parse-method)  
  			(apply str)
  			(spit file-name))

	(let [original (slurp file-name)]
		(spit file-name (str header original)))))

; (build-wrapper java.lang.Integer)




(defn -main [& args]
	(build-wrapper (RSyntaxTextArea. )))


















