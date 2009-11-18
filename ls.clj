(defn glob [dirname] (into [] (.list (new java.io.File dirname))))

(glob "/boot/grub")