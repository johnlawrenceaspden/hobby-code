;; How many words are there in this blog post? What are the most common words?

;; Firstly, how do we retrieve the text?
(use 'clojure.contrib.duck-streams)
(def post (slurp* "http://learnclojure.blogspot.com/2010/02/word-counting-html-page-self.html"))
(count post)
(def fpost (apply str (filter #(not (= % \newline)) post)))
(count fpost)

(use 'clojure.contrib.str-utils)
(take 5 (re-split #"<[^>]*>" post))
(take 20 (re-partition #"<[^>]*>" post))

(defn regexpstospaces [regexp]
  (fn [s] (apply str (interpose " " (re-split regexp s)))))

(def withoutscripts (regexpstospaces #"<script[^>]*>.*?</script>"))
(def withoutstyle   (regexpstospaces #"<style[^>]*>.*?</style>"))
(def withouttags    (regexpstospaces #"<[^>]*?>"))

(count (withouttags (withoutscripts fpost)))
(count (withoutstyle (withoutscripts post)))

(defn withoutscripts [s] (re-gsub #"<script[^>]*>.*?</script>" " " s))
(defn withoutstyle[s]    (re-gsub #"<style[^>]*>.*?</style>" " " s))
(defn withoutspan[s]    (re-gsub #"<span[^>]*>.*?</span>" " " s))
(defn withoutlist[s]    (re-gsub #"<ul[^>]*>.*?</ul>" " " s))

(defn withouttags[s]     (re-gsub #"<[^>]*?>" " " s))
(defn collapsespace [s]  (re-gsub #" +" " " s))
(defn html-deescape-apostrophe [s]  (re-gsub #"&#39;" "'" s))

(html-deescape-apostrophe 
 (collapsespace 
  (withouttags
   (withoutlist
    (withoutspan
     (withoutstyle
      (withoutscripts fpost)))))))
" Learning Clojure: word counting an html page: a self-referential blog post Learning Clojure About Me John Lawrence Aspden View my complete profile Blog Archive Friday, February 26, 2010 word counting an html page: a self-referential blog post hello world 0comments: Post a Comment Home Subscribe to: Post Comments (Atom) Followers "

" Learning Clojure: word counting an html page: a self-referential blog post Learning Clojure About Me John Lawrence Aspden View my complete profile Blog Archive 2010 February word counting an html page: a self-referential blo... Clojure Dojo 4: Symbolic Differentiation Watching a macro as it expands Clojure Dojo 3: From Heron to Newton-Raphson Clojure Dojo 2: What about numbers that aren't 10?... Jihad Scanner Requiring all possible namespaces Using the trace function in clojure.contrib January How the Clojure dojo actually worked in practice. Clojure Dojo: The method of Heron of Alexandria 2009 December Favourite keys for EMACS and SLIME Understanding the REPL xxdiff and hg view with mercurial November classpath Namespaces and Unit Tests Mutable State in the Sudoku Solver Sudoku Solver The Sieve of Eratosthenes Installing Clojure on Ubuntu 9.10 (Karmic Koala) clojure syntax highlighting code October The sequence monad It's lazy, lazy, very very lazy September How it Works: the Monad A Simple Monad Example further macros macros 101 threads and transactional memory a dynamic java with a REPL a minimal web app Fractal tree nested maps and some ways of getting at the keys HTML Formatter Pig Latin Friday, February 26, 2010 word counting an html page: a self-referential blog post hello world 0comments: Post a Comment Home Subscribe to: Post Comments (Atom) Followers "
" Learning Clojure: word counting an html page: a self-referential blog post Learning Clojure About Me John Lawrence Aspden View my complete profile Blog Archive &#9660;&#160; 2010 (10) &#9660;&#160; February (8) word counting an html page: a self-referential blo... Clojure Dojo 4: Symbolic Differentiation Watching a macro as it expands Clojure Dojo 3: From Heron to Newton-Raphson Clojure Dojo 2: What about numbers that aren't 10?... Jihad Scanner Requiring all possible namespaces Using the trace function in clojure.contrib &#9658;&#160; January (2) How the Clojure dojo actually worked in practice. Clojure Dojo: The method of Heron of Alexandria &#9658;&#160; 2009 (23) &#9658;&#160; December (3) Favourite keys for EMACS and SLIME Understanding the REPL xxdiff and hg view with mercurial &#9658;&#160; November (7) classpath Namespaces and Unit Tests Mutable State in the Sudoku Solver Sudoku Solver The Sieve of Eratosthenes Installing Clojure on Ubuntu 9.10 (Karmic Koala) clojure syntax highlighting code &#9658;&#160; October (2) The sequence monad It's lazy, lazy, very very lazy &#9658;&#160; September (11) How it Works: the Monad A Simple Monad Example further macros macros 101 threads and transactional memory a dynamic java with a REPL a minimal web app Fractal tree nested maps and some ways of getting at the keys HTML Formatter Pig Latin Friday, February 26, 2010 word counting an html page: a self-referential blog post hello world Posted by John Lawrence Aspden at 2:37 AM 0comments: Post a Comment Older Post Home Subscribe to: Post Comments (Atom) Followers "
" Learning Clojure: word counting an html page: a self-referential blog post Learning Clojure About Me John Lawrence Aspden View my complete profile Blog Archive &#9660;&#160; 2010 (10) &#9660;&#160; February (8) word counting an html page: a self-referential blo... Clojure Dojo 4: Symbolic Differentiation Watching a macro as it expands Clojure Dojo 3: From Heron to Newton-Raphson Clojure Dojo 2: What about numbers that aren&#39;t 10?... Jihad Scanner Requiring all possible namespaces Using the trace function in clojure.contrib &#9658;&#160; January (2) How the Clojure dojo actually worked in practice. Clojure Dojo: The method of Heron of Alexandria &#9658;&#160; 2009 (23) &#9658;&#160; December (3) Favourite keys for EMACS and SLIME Understanding the REPL xxdiff and hg view with mercurial &#9658;&#160; November (7) classpath Namespaces and Unit Tests Mutable State in the Sudoku Solver Sudoku Solver The Sieve of Eratosthenes Installing Clojure on Ubuntu 9.10 (Karmic Koala) clojure syntax highlighting code &#9658;&#160; October (2) The sequence monad It&#39;s lazy, lazy, very very lazy &#9658;&#160; September (11) How it Works: the Monad A Simple Monad Example further macros macros 101 threads and transactional memory a dynamic java with a REPL a minimal web app Fractal tree nested maps and some ways of getting at the keys HTML Formatter Pig Latin Friday, February 26, 2010 word counting an html page: a self-referential blog post hello world Posted by John Lawrence Aspden at 2:37 AM 0comments: Post a Comment Older Post Home Subscribe to: Post Comments (Atom) Followers "
"                    Learning Clojure: word counting an html page: a self-referential blog post                         Learning Clojure             About Me       John Lawrence Aspden   View my complete profile                Blog Archive        &#9660;&#160;   2010  (10)     &#9660;&#160;   February  (8)    word counting an html page: a self-referential blo...    Clojure Dojo 4: Symbolic Differentiation    Watching a macro as it expands    Clojure Dojo 3: From Heron to Newton-Raphson    Clojure Dojo 2: What about numbers that aren&#39;t 10?...    Jihad Scanner    Requiring all possible namespaces    Using the trace function in clojure.contrib                   &#9658;&#160;           January  (2)    How the Clojure dojo actually worked in practice.    Clojure Dojo: The method of Heron of Alexandria                     &#9658;&#160;           2009  (23)               &#9658;&#160;           December  (3)    Favourite keys for EMACS and SLIME    Understanding the REPL    xxdiff  and hg view with mercurial                   &#9658;&#160;           November  (7)    classpath    Namespaces and Unit Tests    Mutable State in the Sudoku Solver    Sudoku Solver    The Sieve of Eratosthenes    Installing Clojure on Ubuntu 9.10 (Karmic Koala)    clojure syntax highlighting code                   &#9658;&#160;           October  (2)    The sequence monad    It&#39;s lazy, lazy, very very lazy                   &#9658;&#160;           September  (11)    How it Works: the Monad    A Simple Monad Example    further macros    macros 101    threads and transactional memory    a dynamic java with a REPL    a minimal web app    Fractal tree    nested maps and some ways of getting at the keys    HTML Formatter    Pig Latin                          Friday, February 26, 2010      word counting an html page: a self-referential blog post      hello world       Posted by John Lawrence Aspden   at  2:37 AM                                 0comments:                Post a Comment                                 Older Post   Home      Subscribe to: Post Comments (Atom)      Followers                                 "


(#"<div class='post-body entry-content'> <>"

