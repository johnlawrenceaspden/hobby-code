/*
http://duriansoftware.com/joe/An-intro-to-modern-OpenGL.-Chapter-2:-Hello-World:-The-Slideshow.html



*/

#include <stdlib.h>
#include <stdio.h>
#include <GL/glew.h>
#ifdef __APPLE__
#  include <GLUT/glut.h>
#else
#  include <GL/glut.h>
#endif


int make_resources(void)
{
  return 1;
}

static void update_fade_factor(void)
{
}

static void render(void)
{

}


int main(int argc, char** argv)
{
  printf("Hello\n");
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
  glutInitWindowSize(400,300);
  glutCreateWindow("Hello World\n");

  glewInit();
  printf("%d\n",GLEW_VERSION_2_0);
  if(!GLEW_VERSION_2_0){
    printf("GLEW 2.0 unavailable\n");
  }

  if(!make_resources()){
    fprintf(stderr, "failed to load resources\n");
    return 0;
  }

  glutDisplayFunc(&render);
  glutIdleFunc(&update_fade_factor);


  glutMainLoop();
  printf("Goodbye\n");
  return 0;
}
