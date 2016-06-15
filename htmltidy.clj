;; Tidying up HTML with JTidy

;; to dynamically load libraries from maven and clojars
;; must have [com.cemerick/pomegranate "0.3.1"] on your classpath

(require 'cemerick.pomegranate)

;; Here's an example: we get and load weavejester's hiccup library

(cemerick.pomegranate/add-dependencies 
 :coordinates '[[hiccup "1.0.5"]]
 :repositories {"clojars" "http://clojars.org/repo" })

(require '[hiccup.core :as hc])

;; Hiccup is a nice way of writing html

(hc/html [:h1 "hello"]) ;;  "<h1>hello</h1>"

;; But there's no way to pretty print the resulting html, which can get hard to read

(hc/html [:table
          [:thead [:td][:td]]
          [:tr    [:td][:td]]
          [:tr    [:td][:td]]]) ;; "<table><thead><td></td><td></td></thead><tr><td></td><td></td></tr><tr><td></td><td></td></tr></table>"

;; This problem made it to Stack Overflow
;; http://stackoverflow.com/questions/1918901/compojure-html-formatting/1967179#1967179

;; JTidy to the rescue
(cemerick.pomegranate/add-dependencies 
 :coordinates '[[net.sf.jtidy/jtidy "r938"]])


(import '(org.w3c.tidy Tidy))

;; Every time I go anywhere near Java I remember why I hate Java so much 
(defn jtidypp [input]
  (let [forfuckssake (new java.io.StringReader input)
        output (new java.io.StringWriter)
        tidying-thing (new Tidy)]
    (.parse tidying-thing forfuckssake output)
    (str output)))
  

(print (jtidypp "<td></td>"))
;; <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 3.2//EN">
;; <html>
;; <head>
;; <meta name="generator" content=
;; "HTML Tidy for Java (vers. 2009-12-01), see jtidy.sourceforge.net">
;; <title></title>
;; </head>
;; <body>
;; <table>
;; <tr>
;; <td></td>
;; </tr>
;; </table>
;; </body>
;; </html> 

;; This is perhaps a little over-tidied for my needs, and indentation would be nice

(defn jtidypp [input]
  (let [forfuckssake (new java.io.StringReader input)
        output (new java.io.StringWriter)
        tidying-thing (new Tidy)]
    (.setSmartIndent tidying-thing  true)
    (.setPrintBodyOnly tidying-thing true)
    (.parse tidying-thing forfuckssake output)
    (str output)))

(print (jtidypp "<td></td>"))
;; <table>
;;   <tr>
;;     <td>
;;     </td>
;;   </tr>
;; </table>

;; This is ok, but I don't want the bloody thing adding tags, I just want it to be indented properly.

;; On the original example it actually does pretty much what I want, but I don't trust it!
(print (jtidypp (hc/html [:table
                          [:thead [:td][:td]]
                          [:tr    [:td][:td]]
                          [:tr    [:td][:td]]])))
;; <table>
;;   <thead>
;;     <tr>
;;       <td>
;;       </td>

;;       <td>
;;       </td>
;;     </tr>
;;   </thead>

;;   <tr>
;;     <td>
;;     </td>

;;     <td>
;;     </td>
;;   </tr>

;;   <tr>
;;     <td>
;;     </td>

;;     <td>
;;     </td>
;;   </tr>
;; </table>

;; There's another suggestion to use Jsoup


(cemerick.pomegranate/add-dependencies 
 :coordinates '[[org.jsoup/jsoup "1.9.2"]])

(import '(org.jsoup Jsoup))

;; Jsoup can be called in a sane manner. I suspect that the authors aren't Java men.

(defn jsouppp [html]
  (. (. (Jsoup/parseBodyFragment html) body) html))

;; But it's even more interfering than the last one.

(jsouppp "<td></td>") ;; ""  --  no output! it's just thrown the damned thing away
(jsouppp "<td>a</td>") ;; "a"  --  weird...
(jsouppp "<table><td></td></table>") ;; "<table>\n <tbody>\n  <tr>\n   <td></td>\n  </tr>\n </tbody>\n</table>"

(print (jsouppp "<table><td></td></table>"))
;; <table>
;;  <tbody>
;;   <tr>
;;    <td></td>
;;   </tr>
;;  </tbody>
;; </table>


;; On the original I like the fact that it's clever enough to put <td></td> on one line

(print (jsouppp (hc/html [:table
                          [:thead [:td][:td]]
                          [:tr    [:td][:td]]
                          [:tr    [:td][:td]]]))) 

;; <table>
;;  <thead>
;;   <tr>
;;    <td></td>
;;    <td></td>
;;   </tr>
;;  </thead>
;;  <tbody>
;;   <tr>
;;    <td></td>
;;    <td></td>
;;   </tr>
;;   <tr>
;;    <td></td>
;;    <td></td>
;;   </tr>
;;  </tbody>
;; </table>


(print (jsouppp (hc/html [:table {:class "table"}
                          [:thead {:class "head"}[:td "alpha"][:td "beta"]]
                          [:tr  {:class "row"}  [:td "alpha"] [:td "beta"]]
                          [:tr  {:class "row"}  [:td "alpha"] [:td "beta"]]])))

;; <table class="table">
;;  <thead class="head">
;;   <tr>
;;    <td>alpha</td>
;;    <td>beta</td>
;;   </tr>
;;  </thead>
;;  <tbody>
;;   <tr class="row">
;;    <td>alpha</td>
;;    <td>beta</td>
;;   </tr>
;;   <tr class="row">
;;    <td>alpha</td>
;;    <td>beta</td>
;;   </tr>
;;  </tbody>
;; </table>

(print (jtidypp (hc/html [:table {:class "table"}
                          [:thead {:class "head"}[:td "alpha"][:td "beta"]]
                          [:tr  {:class "row"}  [:td "alpha"] [:td "beta"]]
                          [:tr  {:class "row"}  [:td "alpha"] [:td "beta"]]])))


;; <table class="table">
;;   <thead class="head">
;;     <tr>
;;       <td>alpha</td>
;;
;;       <td>beta</td>
;;     </tr>
;;   </thead>
;;
;;   <tr class="row">
;;     <td>alpha</td>
;;
;;     <td>beta</td>
;;   </tr>
;;
;;   <tr class="row">
;;     <td>alpha</td>
;;
;;     <td>beta</td>
;;   </tr>
;; </table>
