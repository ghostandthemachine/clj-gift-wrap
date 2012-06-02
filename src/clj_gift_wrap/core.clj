(ns clj-gift-wrap.core
	(:use [clojure.reflect]
		  [clojure.pprint]
		  [inflections.core])
	(:import 
		(org.fife.ui.rtextarea RTextArea Macro)
		(org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants TokenMakerFactory))
	(:require 
		  [clojure.string :as s]
		  [clojure.contrib.string :as cs]))

(def alphabet ["x" "y" "z" "w" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"])
(def global-class-name (atom nil))

(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

(defn single-boolean-arg?
	[fn-name]
	(= "boolean" (str (s/join (take 7 (drop 1 (str fn-name)))))))

(def t {:parameter-types "[boolean]" :return-type "boolean"})

(defn boolean? 
	[m]
	(or 
		(single-boolean-arg? (:parameter-types m))
		(= "boolean" (str (s/join (take 7 (str (:return-type m))))))))

(defn setter?
	[fn-name]
	(or 
		(= "put" (str (s/join (take 3 (str fn-name)))))
		(= "set" (str (s/join (take 3 (str fn-name)))))))

(defn should-have-bang? 
	[fn-name]
	(or
		(= "start" (str (s/join (take 5 (str fn-name)))))
		(= "begin" (str (s/join (take 5 (str fn-name)))))
		(= "end" (str (s/join (take 3 (str fn-name)))))
		(= "add" (str (s/join (take 3 (str fn-name))))))
	)

(defn getter?
	[m]
	(and
		(not (boolean? m))
		(= "get" (str (s/join (take 3 (str (:name m))))))))


(defn slice-count 
	[fn-name]
	(cond
		(or
			(= "put" (str (s/join (take 3 (str fn-name)))))
			(= "add" (str (s/join (take 3 (str fn-name))))))
		3))

(defn get-fn-type 
	[m]
	(cond
	  (getter? m)
	  :getter
	  (and
	  	(setter? (:name m))
	  	(not (single-boolean-arg? (:parameter-types m))))
	  :setter
	  (or 
	  	(boolean? m)
	  	(single-boolean-arg? (:parameter-types m)))
	  :boolean
	  (should-have-bang? (:name m))
	  :should-have-bang
	  :else 
	  	:unknown))

(defn parse-args
	[arguments]
	(let [num-args (count arguments)]
		(apply str (for [i (range num-args)] (str " " (nth alphabet i))))))

(defn public? 
	[m]
	(and 
		(not (in? (:flags m) :private))
		(not (in? (:flags m) :protected))))

(defn static?
	[m]
	(in? (:flags m) :static))

(defn synthetic?
	[m]
	(in? (:flags m) :synthetic))

(defn format-setter
	[method-name]
	(str (s/join (drop 4 (hyphenize method-name))) "!"))

(defn format-getter
	[method-name]
	(str (s/join (drop 4 (hyphenize method-name)))))

(defn format-boolean 
	[m]

	(let [method-name (str (:name m))]
		(cond
	  		(= "is" (str (s/join (take 2 method-name))))
				(str (hyphenize (str (s/join (drop 2 method-name)))) "?")
	  		(= "has" (str (s/join (take 3 method-name))))
				(str (hyphenize (str (s/join (drop 3 method-name)))) "?")
	  		(= "set" (str (s/join (take 3 method-name))))
				(str (hyphenize (str (s/join (drop 3 method-name)))) "?")
			(and 
				(boolean? m)
				(= "get" (str (s/join (take 3 method-name)))))
				(str (hyphenize (str (s/join (drop 3 method-name)))) "?")
			:else
				(str (hyphenize (str method-name)) "?"))))

(defn format-bang
	[method-name]
	(str (hyphenize method-name) "!"))

(defn format-slice
	[method-name n]
	(str (s/join (drop n method-name))))

(defn format-unkown
	[method-name]
	(str (hyphenize method-name)))

(defn parse-fn-name
	[m]
	(let [type (get-fn-type m)
		  	name (cond
	  				(= type :setter)
	  					(format-setter (str (:name m)))
	  				(= type :getter)
	  					(format-getter (str (:name m)))
	  				(= type :boolean)
						(format-boolean m)
	  				(= type :unkown)
	  					(format-unkown (str (:name m)))
	  				(= type :should-have-bang)
	  					(format-bang (str (:name m)))
	  				:else 
	  					(format-unkown (str (:name m))))]
 		(str name "\n")))
		
(defn public-method-to-string 
	[m]
	(str "(defn " (parse-fn-name m)
		"\"args: " (str (:parameter-types m))"\n"
		"flags: " (str (:flags m))"\n"
		"return-type: " (str (:return-type m))"\"\n"
		"\t[obj" (parse-args (:parameter-types m)) "]\n"
		"\t(." (str (:name m)) " obj" (parse-args (:parameter-types m)) "))\n\n"))

(defn static-method-to-string 
	[m class-name]
	(if (public? m)
		(str "(defn " (parse-fn-name m)
		"\"args: " (str (:parameter-types m))"\n"
		"flags: " (str (:flags m))"\n"
		"return-type: " (str (:return-type m))"\"\n"
		"\t[obj" (parse-args (:parameter-types m)) "]\n"
		"\t(" (:declaring-class m) "/" (str (:name m)) (parse-args (:parameter-types m)) "))\n\n")))

(defn parse-method
	[method]
	(let [m method
		  class-name @global-class-name]
		(cond 
			(and 
				(public? m)
				(not (static? m))
				(not (synthetic? m)))
			(public-method-to-string m)
			(and 
				(static? m)
				(not (synthetic? m)))
			(static-method-to-string m class-name)
			:else 
			"")))

(defn build-wrapper
	[obj]
	(swap! global-class-name (fn [_] (s/join (drop 6 (str (type obj))))))
	(let [members (:members (reflect obj))
		  header (str "(ns " (cs/replace-char \. \_ (underscore @global-class-name)) ".core)\n\n\n")
		  file-name (str (cs/replace-char \. \_ (underscore @global-class-name)) ".clj")]
		(->> members
			(map parse-method)  
  			(apply str)
  			(spit file-name))

	(let [original (slurp file-name)]
		(spit file-name (str header original)))))


(defn -main [& args]
	(build-wrapper (Macro. ))
	(build-wrapper (RTextArea. ))
	(build-wrapper (RSyntaxTextArea. ))
	)




