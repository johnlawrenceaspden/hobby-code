(ns lessons
  (:use [compojure]))

(defn strings->row [slst]
  [:tr (map (fn[x] [:td x]) slst)])

(defn stringss->table [slstlst]
  [:table {:border 1}
   (map strings->row slstlst)])







(def course
     '(("Lesson" "Learning Objective" "Outline of Lesson" "Keywords" "What Pupils Produce" "Success Criteria")

   ("lesson 1" "introduction to artist" "look at pictures" "pens, crayons")
   ("lesson 2" "mileu" "examine conditions" "pens, crayons")))

(defn lessonplan []
  (html
   [:head
    [:title "Lesson Plan"]]
   [:body
    [:h1 "Plan"]
    (stringss->table course)
]))



(defn lessonplan []
     (html
      [:head
       [:title "Bishop Barrington School: Art Department"]]
      [:body
       [:h1 "Scheme of Work: Jon Burgerman - Level 5 to Level 6"]
       [:table {:border 1}
        [:tr
         [:td "Lesson"]
         [:td "Learning Objective" ]
         [:td "Outline of Lesson" ]
         [:td "Keywords" ]
         [:td "What Pupils Produce" ]
         [:td "Success Criteria" ]
         ]
        ]]))


"Lesson One:"



"Pupils look at examples of work"

"Pupils create a title"

"•"

"Good use of formal"

"•"

"Today we are learning"

"•"

"Style"

"Introduction To"

"by Jon Burgerman."

"elements."

"how to take creative"

"page in their"

"•"

"Content"

"Jon Burgerman’s Art."

"sketchbook based on"

"risks when exploring,"

"•"

"View & Discuss (Content, Form,"

"•"

"Clear Burgerman"

"•"

"Form"

"experimenting and"

"Proces s, Mood)."

"the work of Jon"

"influence."

"•"

"Proces s"

"Burgerman - using"

"responding to ideas."

"•"

"Title page in sketchbook -"

"•"

"Fills the page."

"•"

"Mood"

"appropriate materials,"

"working A3 size."

"•"

"Clear title."

"•"

"Loose"

"techniques and"

"•"

"Distinctive"

"processes."

"•"

"Contemporary"

"•"

"Influence"

"Lesson Two:"

"•"

"Today we are learning"

"•"

"Pupils look at doodles they have"

"•"

"Doodle"

"•"

"Pupils create a double"

"•"

"Pupils write own"

"Doodling!"

"how to select images to"

"brought in and examples of"

"page spread in their"

"success criteria in"

"•"

"Loose"

"demonstrate our re-"

"Burgerman’s work."

"book showing their own"

"lesson after looking at"

"•"

"Style"

"search into the work of"

"research into"

"example book."

"•"

"They choose sections to enlarge"

"•"

"Select"

"an artist."

"and develop into small"

"Burgerman’s work."

"•"

"Enlarge"

"Burgerman style drawings."

"•"

"They include some of"

"•"

"Modify"

"their own doodles and"

"•"

"Display"

"enlarge sections from"

"•"

"Layout"

"some and add colour in"

"Burgerman’s style."

"Lesson Three:"

"•"

"Pupils look at the cultural context"

"•"

"Pupils create a page in"

"•"

"Pupils write own"

"•"

"Today we are learning"

"•"

"Context"

"Cultural Context."

"how to illustrate our"

"of Burgerman’s work and"

"their sk etchbook looking"

"success criteria in"

"•"

"Contemporary"

"at the meaning of con-"

"lesson after looking at"

"understanding of the"

"dev elop an understanding of the"

"•"

"Style"

"place it has in contemporary art."

"text and res earching the"

"example book."

"cultural and social"

"•"

"Function"

"work of Burgerman’s"

"context of the artwork"

"•"

"They view and discuss work"

"•"

"Influence"

"we are studying."

"peers."

"created by Burgerman’s peers"

"•"

"Use"

"and comment no the type of"

"•"

"They include notes that"

"•"

"Purpose"

"people it would appeal to."

"communicate their own"

"personal response to"

"the work."

"Lesson Four:"

"•"

"Today we are learning"

"•"

"Pupils complete a workshop that"

"•"

"Technique"

"•"

"Pupils create an A2"

"•"

"Each technique has its"

"Tec hniques & Media."

"how to select and use"

"explores a variety of techniques"

"•"

"Proces s"

"sheet that shows a vari-"

"own success criteria for"

"appropriate materials"

"they can use when c reating their"

"ety of experiments with"

"pupils to work towards."

"•"

"Media"

"and techniques when"

"work."

"techniques processes"

"•"

"They refer to these"

"•"

"Material"

"creating works of art."

"and media."

"•"

"They explore acrylics, pens ,"

"within their annotations"

"•"

"Evaluation"

"computers, pencils, printing and"

"•"

"They clearly make"

"and evaluations."

"•"

"Health & Safety"

"mak e reasoned evaluations"

"notes and evaluate"

"about their outcomes."

"each technique and add"

"relevant H&S advice."

"•"

"They comment on relevant H&S"

"practices."

"Jonathan Carney - 2008"

"Bishop Barrington School: Art Department"

"Scheme of Work:"

"Jon Burgerman - Level 5 to Level 6"

"Lesson"

"Learning Objective"

"Outline of Lesson"

"Keywords"

"What Pupils Produce"

"Success Criteria"

"Lesson Five:"

"•"

"Pupils choose a design from"

"•"

"Pupils create a variety"

"•"

"Pupils write their own"

"•"

"Today we are learning"

"•"

"Style"

"Surfaces."

"their doodle sk etchbook to"

"succes s criteria based"

"how to accept creative"

"of small scale works on"

"•"

"Loose"

"recreate on a variety of surfaces."

"a range of surfaces."

"on the surface they are"

"risks and explore ideas"

"•"

"Distinctive"

"using."

"independently and"

"•"

"They use a range of techniques"

"•"

"They stick these into"

"•"

"Influence"

"inventively."

"and media to create their work."

"their sk etchbook and"

"•"

"Proces s"

"evaluate their suc-"

"•"

"Media"

"cesses and failures."

"•"

"Technique"

"•"

"They explore a variety"

"•"

"Enlarge"

"of techniques."

"•"

"Develop"

"Lesson Six:"

"•"

"Today we are learning"

"•"

"Pupils look at the 3D characters"

"•"

"3D"

"•"

"Pupils choose one of"

"•"

"Use of a variety of tools."

"3D Versions Of Characters."

"how to use resources"

"created by Burgerman and adapt"

"their doodles (or a"

"•"

"Develop"

"•"

"Experiments with scale."

"imaginatively to"

"their own doodles into 3D"

"Burgerman one) to"

"•"

"Model"

"•"

"Clear influence of"

"develop, design and"

"models."

"develop into a small"

"•"

"Adapt"

"Burgerman’s style."

"make work."

"•"

"They explore the use of"

"scale 3D model."

"•"

"Look at examples of"

"•"

"Refine"

"modelling clay and write about"

"•"

"They take photos of the"

"Burgerman’s work."

"•"

"Experiment"

"the problems they have when"

"process to create a"

"•"

"Experiments with use of"

"creating something 3D from"

"page in their book that"

"colours."

"something 2D."

"documents their"

"experiment."

"Lesson Seven:"

"•"

"Pupils have a day or 1/2 day"

"•"

"Pupils ask Jon to make"

"•"

"Show how Jon"

"•"

"Today we are learning"

"•"

"Technique"

"Artist Workshop*"

"how to consider and"

"session with Jon Burgerman."

"notes in their"

"Burgerman has"

"•"

"Style"

"sketchbooks and tak e"

"influenced your work."

"discuss the ideas"

"•"

"They view his work and discus"

"•"

"Practice"

"photos of him at work."

"methods and"

"his style and use of techniques."

"•"

"Take plenty of photos."

"•"

"Discuss"

"techniques used by an"

"•"

"They produce a page in"

"•"

"He helps pupils develop their"

"•"

"Use the ideas and"

"•"

"artist."

"their book documenting"

"experience to help"

"own work and demonstrates his"

"the day and detailing"

"own practice to the class."

"inform further"

"their experience."

"development."

"Lesson Eight:"

"•"

"Today we are learning"

"•"

"Pupils look at their work and"

"•"

"Design"

"•"

"A3 pages in their book"

"•"

"Variety of final ideas."

"Design Ideas."

"how to apply our"

"research and develop an idea for"

"•"

"Develop"

"showing the"

"•"

"Explore 2D and 3D"

"knowledge to realise our"

"a final piece inspired by the work"

"development of a final"

"options."

"•"

"Creativity"

"intentions."

"of Burgerman."

"design - with"

"•"

"Clear link with research"

"•"

"Surface"

"appropriate and"

"•"

"They develop their idea and"

"and own drawings."

"•"

"Individuality"

"consider how they might us e"

"relevant notes."

"•"

"Appropriate use of"

"•"

"Found Objects"

"different surfaces to create their"

"•"

"An A3 page looking at"

"colour, line, shape and"

"•"

"Context"

"final piece upon."

"the variety of surfaces"

"form."

"•"

"Purpose"

"they can use to create"

"their final piece."

"*Subject to arrangement."

"Bishop Barrington School: Art Department"

"Scheme of Work:"

"Jon Burgerman - Level 5 to Level 6"

"Lesson"

"Learning Objective"

"Outline of Lesson"

"Keywords"

"What Pupils Produce"

"Success Criteria"

"Lesson Nine:"

"•"

"Pupils develop a final idea and"

"•"

"An A3 page that shows"

"•"

"Clear development of"

"•"

"Today we are learning"

"•"

"Resources"

"Final Design"

"experiment with arrange of"

"idea."

"how to apply our"

"a clear plan of their final"

"•"

"Imagination"

"colours/media until they decide"

"piece - including any"

"knowledge to realise our"

"•"

"Influence of research"

"•"

"Experiment"

"on an outcome."

"intentions."

"techniques that they will"

"into style of artist."

"•"

"Develop"

"be using."

"•"

"They look at and evaluate their"

"•"

"Think about a variety of"

"•"

"Evaluation"

"own work and that of other’s in"

"•"

"An explanation of the"

"media/processes"

"•"

"Judgement"

"the class and share good"

"purpose and meaning of"

"•"

"Choosing appropriate"

"•"

"Creativity"

"practice and ideas."

"their work."

"materials/techniques."

"•"

"Purpose"

"Lesson Ten:"

"•"

"Today we are learning"

"•"

"Pupils create their final piece of"

"•"

"Quality"

"•"

"A ‘quality’ final piece"

"•"

"Well presented and"

"Final Piece."

"how to use resources"

"work - referring to their designs"

"that shows clear artist"

"made piece of work."

"•"

"Process"

"imaginatively to"

"and using appropriate"

"influence and a good"

"•"

"Taken obvious care and"

"•"

"Meaning"

"develop, design and"

"techniques and processes."

"use of a variety of tech-"

"attention when"

"•"

"Title"

"make work."

"•"

"They name their work and write"

"niques and media."

"producing work."

"•"

"Independence"

"a small explanation to acc om-"

"•"

"Own success criteria"

"•"

"Critical Understanding"

"pany their work that explains the"

"written during"

"thoughts, ideas and meanings"

"development of final"

"behind it."

"piece."

"Lesson Eleven:"

"•"

"Pupils scan their work and save"

"•"

"A postcard of their final"

"•"

"Explore arrange of"

"•"

"Today we are learning"

"•"

"Use"

"Sharing Our Work."

"how to accept creative"

"it for use in a variety of ways."

"piece - with explanation"

"options for how your"

"•"

"Development"

"and title on the back."

"design c an be used."

"risks and explore ideas"

"•"

"They create a simple postcard"

"•"

"Creativity"

"independently and"

"with their explanation on the re-"

"•"

"A range of ideas of how"

"•"

"Quality postcard with"

"•"

"Adapt"

"inventively."

"verse and upload their work to"

"their design can be"

"clear explanation on"

"•"

"Realis ing Intentions"

"the school website for use as"

"used or adapted for"

"reverse."

"des ktops /mobile phone wallpa-"

"other uses."

"per."

"•"

"The teacher c reates a simple"

"photobook of the work."

"Lesson Twelve:"

"•"

"Today we are learning"

"•"

"Pupils record their evaluations to"

"•"

"Reasoned"

"•"

"A written ev aluation -"

"•"

"Pupils discuss"

"Evaluations."

"how to provide"

"Crazytalk."

"•"

"Critical"

"they will read this when"

"evaluating and come up"

"reasoned evaluations of"

"recording in Craz ytalk."

"with their own ideas of a"

"•"

"These videos will be combined"

"•"

"Analyse"

"the purpose and mean-"

"with their scanned images to"

"succes sful evaluation."

"•"

"A Podcast that contains"

"•"

"Meaning"

"ing of our work."

"form a Podcast that they can"

"all evaluations."

"•"

"Purpose"

"download from home and share"

"•"

"Page in sketchbook"

"•"

"Confidence"

"with their family and friends ."

"about why we have"

"•"

"Questioning"

"evaluated in this way."

"Jonathan Carney - 2008"

"Bishop Barrington School: Art Department"

"Scheme of Work:"

"Jon Burgerman - Level 5 to Level 6"

"Framework Of Personal, Learning & Thinking Skills"

"Independent Enquirers Creative Thinkers Reflective Learners"

"Process and evaluate information in investigations,"

"Think creatively by generating and exploring ideas,"

"Evaluate strengths and limitations, set realistic goals"

"planning what to do and how to do it. Take informed"

"making original connections. Try different ways to"

"with success criteria. Monitor own performance and"

"and well-reasoned decisions and recognise different"

"tackle problems. Work with others to find imaginative"

"progress, inviting feedback from others and making"

"beliefs and attitudes."

"solutions and outcomes of value."

"changes to further learning."

"Evidence:"

"Evidence:"

"Evidence:"

"•"

"Use of Project Booklet to plan and carry out researc h -"

"•"

"Use of sketchbook to record own ideas in respons e to"

"•"

"Regular use of self and peer evaluations - sheets"

"appreciation that poor research will effect standard of"

"research"

"attached to relevant piece of work."

"practical work."

"•"

"Development of ideas into a ‘quality’ final piece."

"•"

"Writing of own success criteria during lessons."

"•"

"Annotations in sketchbooks - analysing and evaluating"

"•"

"Annotations in sk etchbook showing links between own"

"•"

"Reviewing of ideas and using evaluations to help"

"pieces of artwork and ideas."

"work and that of others."

"develop ideas into a final piece."

"•"

"Pages in sketchbook looking at historical and cultural"

"•"

"Development of a variety of ideas that are adapted and"

"•"

"Response to verbal feedback from teac her during"

"context of the work being studied."

"refined in response to evaluations and feedback."

"lessons."

"•"

"Create an original conclusion, final piece, and provide"

"•"

"Workshops offering use of a variety of techniques -"

"well reasoned annotations explaining the work."

"adaptation of own ideas in response to these s essions."

"Team Workers Self-Managers Effective Participators"

"Work confidently with others, adapt to different"

"Organise themselves, showing personal responsibility,"

"Actively engage with issues that affect them and those"

"contexts and take responsibility for themselves. Listen"

"initiative, creativity and enterprise with a commitment"

"around them. Play a full part in the life of school by"

"and take account of different views. Form collaborative"

"to learning and self-improvement. Actively embrace"

"taking responsible action to bring improvements for"

"relationships, resolving issues to meet agreed"

"change, respond positively to new priorities, cope with"

"others as well as themselves."

"outcomes."

"challenges and look for opportunities."

"Evidence:"

"Evidence:"

"Evidence:"

"•"

"Reach agreement with teacher over final piece."

"•"

"Use of Project Booklet to effectively organise own time"

"and plan/carry out own research."

"•"

"Adaptation behaviour and practice during workshops"

"and lessons."

"•"

"Taking of creative ris ks when using materials and"

"creating original ideas."

"•"

"Fairness and consideration for others shown in"

"classroom practice and general behav iour."

"•"

"Respond to evaluations, feedback and criticism"

"•"

"Use of Project Booklet to take responsibility for own"

"positively and adapt/refine work where asked to do so."

"work and progress."

"•"

"Ask for help and feedback when required - use this to"

"help develop practical work."

"•"

"Write reasoned and constructive feedback on peer"

"evaluation sheets."

"Jonathan Carney - 2008"

