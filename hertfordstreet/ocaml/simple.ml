(*This program is taken from the wikipedia ocaml page 
and should be compiled with
$ ocamlc -I +lablGL lablglut.cma lablgl.cma simple.ml -o simple *)

let _ =
  ignore ( Glut.init Sys.argv );
  Glut.initDisplayMode ~double_buffer:true ();
  ignore (Glut.createWindow ~title:"OpenGL Demo");
  let angle t = 10. *. t *. t in
  let render () =
    GlClear.clear [ `color ];
    GlMat.load_identity ();
    GlMat.rotate ~angle: (angle (Sys.time ())) ~z:1. ();
    GlDraw.begins `triangles;
    List.iter GlDraw.vertex2 [-1., -1.; 0., 1.; 1., -1.];
    GlDraw.ends ();
    Glut.swapBuffers () in
GlMat.mode `modelview;
Glut.displayFunc ~cb:render;
Glut.idleFunc ~cb: (Some Glut.postRedisplay);
Glut.mainLoop ()
