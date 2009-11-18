;; hello world dialog box

(import javax.swing JOptionPane)

(. javax.swing.JOptionPane (showMessageDialog nil "Hello World"))

(. javax.swing.JOptionPane (showMessageDialog nil "hi there" "Hello World" javax.swing.JOptionPane/WARNING_MESSAGE))



//default title and icon
JOptionPane.showMessageDialog(frame,
    "Eggs are not supposed to be green.", 
    "Message");


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"Hello World"//Custom button text
Object[] options = {"Yes, please",
                    "No, thanks",
                    "No eggs, no ham!"};
int n = JOptionPane.showOptionDialog(frame,
    "Would you like some green eggs to go "
    + "with that ham?",
    "A Silly Question",
    JOptionPane.YES_NO_CANCEL_OPTION,
    JOptionPane.QUESTION_MESSAGE,
    null,
    options,
    options[2]);




(def options (to-array ["Yes, please"
			"No, thanks"
			"No eggs, no ham!"]))
(. javax.swing.JOptionPane (showOptionDialog nil 
					     (str "Would you like some green eggs to go " "with that ham?")
					      "A Silly Question"
					      javax.swing.JOptionPane/YES_NO_CANCEL_OPTION
					      javax.swing.JOptionPane/QUESTION_MESSAGE
					      nil,
					      options
					      (get options 2)))




