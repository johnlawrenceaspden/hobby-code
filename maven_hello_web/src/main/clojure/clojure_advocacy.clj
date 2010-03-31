(ns clojure-advocacy
  [:use compojure])

(def clojure-page
     (html [:head [:title "Clojure"]]
           [:body

            [:h1 "Clojure"]
            [:p "All the advantages of LISP"]
            [:p "All the advantages of Java"]
            [:p "Massively parallel in its bones."]
            [:p "A new language for a new age"]

            [:h2 "LISP"]
            [:blockquote "The greatest single programming language ever designed"]           
            [:blockquote "Alan Kay"]
            [:p "In his famous essay, 'Beating the Averages', Paul Graham describes how he and his partner Robert Morris managed to beat hundreds of other companies, many with large, well funded programming teams, to create Yahoo Stores."]
            [:p "They sold their company, Viaweb, to Yahoo for $ 48 000 000. No one could believe that there had only been two of them."]
            [:p "In his essay, he takes little credit for his work. He describes how he could do in days what took his competition months."]
            [:p "He describes a truly frightening speed of development."]
            [:p "He credits it all to LISP. A language most people think of as a relic from the age of Artificial Intelligence."]
            [:p "So it is: The language that the brightest people in the world chose to attack the hardest problems in the world."]
            [:p "People like Peter Norvig, Technical Director of Google, who literally wrote the book on AI."]
            [:p "People like Alan Kay, the inventor of Object Orientation and the modern computer interface."]

            [:h2  "Parallelism"]
            [:p "We are entering a new age, when we can no longer rely on processor speed doubling reliably year in, year out."]
            [:p "The era of Moore's law is over. If that were not the case, computer clock speeds would now be around 20GHz."]
            [:p "The processors of six years hence are on the drawing boards now. They'll run at 3 GHz, just like today's."]
            [:p "What they will have is masses and masses of cores. Twenty or forty to a chip. And those numbers will be getting bigger."]
            [:p "We don't know how to write multi-threaded code! Object orientation doesn't work."]
            [:p "This can be an opportunity if we're ready for it! But it needs a different programming paradigm. "]
            [:p "The best hope at the moment is called Software Transactional Memory. We treat memory like a database, and allow the database to coordinate the different threads. We know this works because that's how large web sites work already."]
            [:p "STM programs can implement multi-threaded algorithms without any need for locks or synchronization."]
            [:p "On multi-cored machines they scale effortlessly. Things which can be done in parallel are, and the speed of the program scales with the number of cores."]

            [:h2 "Java"]
            [:blockquote
             "We were not out to win over the Lisp programmers; we were after the C++ programmers. "
             "We managed to drag a lot of them about halfway to Lisp."]
            [:blockquote "- Guy Steele, Java spec co-author" ]
            [:p "Java is the world standard for business computing. Designed by James Gosling and Guy Steele as a LISP for the masses in an age when commercial code was written in C, assembler, COBOL or FORTRAN, it succeeded beyond their wildest dreams."]
            [:p "Java boasts a beautiful fast virtual machine, and a vast ecosystem of libraries for almost any conceivable task. And it's open source software, with a vast community maintaining and supporting it. It's not going anywhere."]
            [:p "This has prompted the growth of many JVM languages, which leverage the VM and the libraries, the best arguments for using Java, and use them to create modern, powerful languages which use Java in the same way that C uses machine code."]
            [:p "Amongst these are Scala, Groovy, JRuby, and Clojure. Of these the most famous is Clojure."]

            [:h1 "The Solution?"]
            [:p "Clojure has STM at its heart. Clojure is a LISP. Clojure is a JVM language specifically designed to leverage Java's libraries."]
            [:p "Clojure is an easier way to write Java! But it's also the latest example of the most powerful family of languages there has ever been. And it's leading the world into the multi-core future that we're only just beginning to realise is already here."]



]))


