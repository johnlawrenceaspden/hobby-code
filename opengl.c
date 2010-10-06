/*
http://duriansoftware.com/joe/An-intro-to-modern-OpenGL.-Chapter-2:-Hello-World:-The-Slideshow.html

Ubuntu 10.04 on dell-mini

Install glut and glew libraries
sudo apt-get install freeglut3-dev libglew1.5-dev

$ cc -lglut -lGLEW opengl.c && ./a.out


*/

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <GL/glew.h>
#ifdef __APPLE__
#  include <GLUT/glut.h>
#else
#  include <GL/glut.h>
#endif

static GLuint make_texture(const char *filename)
{
  GLuint texture;
  int width, height;
  void *pixels = 0;

  pixels=malloc(10000);
  width=100;
  height=100;
  if (!pixels){
    fprintf(stderr, "pixels is null\n");
    return 0;
  }

  glGenTextures(1, &texture);
  glBindTexture(GL_TEXTURE_2D, texture);

  glTexImage2D(
               GL_TEXTURE_2D, 0,
               GL_RGB8,
               width, height, 0,
               GL_BGR, GL_UNSIGNED_BYTE,
               pixels
               );
  
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,     GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,     GL_CLAMP_TO_EDGE);
  
  free(pixels);
  return texture;


}
void *file_contents(const char *filename, GLint *length)
{
    FILE *f = fopen(filename, "r");
    void *buffer;

    if (!f) {
        fprintf(stderr, "Unable to open %s for reading\n", filename);
        return NULL;
    }

    fseek(f, 0, SEEK_END);
    *length = ftell(f);
    fseek(f, 0, SEEK_SET);

    buffer = malloc(*length+1);
    *length = fread(buffer, 1, *length, f);
    fclose(f);
    ((char*)buffer)[*length] = '\0';

    return buffer;
}



static struct {
  GLuint vertex_buffer, element_buffer;
  GLuint textures[2];

  GLuint vertex_shader, fragment_shader, program;

  struct {
    GLint fade_factor;
    GLint textures[2];
  } uniforms;

  struct {
    GLint position;
  } attributes;

  GLfloat fade_factor;

} g_resources;

static void show_info_log(
    GLuint object,
    PFNGLGETSHADERIVPROC glGet__iv,
    PFNGLGETSHADERINFOLOGPROC glGet__InfoLog
)
{
    GLint log_length;
    char *log;

    glGet__iv(object, GL_INFO_LOG_LENGTH, &log_length);
    log = malloc(log_length);
    glGet__InfoLog(object, log_length, NULL, log);
    fprintf(stderr, "%s", log);
    free(log);
}


static GLuint make_shader(GLenum type, const char *filename)
{
  GLint length;
  GLchar *source = file_contents(filename, &length);
  GLuint shader;
  GLint shader_ok;

  if (!source){
    fprintf(stderr, "Failed to load file\n");
    return 0;
  }

  shader = glCreateShader(type);
  glShaderSource(shader, 1, (const GLchar**)&source, &length);
  free(source);
  glCompileShader(shader);

  glGetShaderiv(shader, GL_COMPILE_STATUS, &shader_ok);
  if (!shader_ok) {
    fprintf(stderr, "Failed to compile %s:\n", filename);
    show_info_log(shader, glGetShaderiv, glGetShaderInfoLog);
    glDeleteShader(shader);
    return 0;
  }
  return shader;

}



static GLuint make_program(GLuint vertex_shader, GLuint fragment_shader)
{
  GLint program_ok;

  GLuint program = glCreateProgram();
  glAttachShader(program, vertex_shader);
  glAttachShader(program, fragment_shader);
  glLinkProgram(program);

  glGetProgramiv(program, GL_LINK_STATUS, &program_ok);
  if (!program_ok) {
    fprintf(stderr, "Failed to link shader program:\n");
    show_info_log(program, glGetProgramiv, glGetProgramInfoLog);
    glDeleteProgram(program);
    return 0;
  }
  return program;
}





static GLuint make_buffer(
 GLenum target, 
 const void *buffer_data, 
 GLsizei buffer_size )
{
  GLuint buffer;
  glGenBuffers(1, &buffer);
  glBindBuffer(target, buffer);
  glBufferData(target, buffer_size, buffer_data, GL_STATIC_DRAW);
  return buffer;
}

static const GLfloat g_vertex_buffer_data[] = {
  -1.0f, -1.0f,
  1.0f, -1.0f,
  -1.0f,  1.0f,
  1.0f,  1.0f,
};

static const GLushort g_element_buffer_data[] = {0, 1, 2, 3};

static int make_resources(void)
{
  g_resources.vertex_buffer = make_buffer(
                                          GL_ARRAY_BUFFER,
                                          g_vertex_buffer_data,
                                          sizeof(g_vertex_buffer_data)
                                          );
  g_resources.element_buffer = make_buffer(
                                           GL_ELEMENT_ARRAY_BUFFER,
                                           g_element_buffer_data,
                                           sizeof(g_element_buffer_data)
                                           );
  g_resources.textures[0] = make_texture("ignore.png");
  g_resources.textures[1] = make_texture("ignore.png");

  g_resources.vertex_shader = make_shader(
                                         GL_VERTEX_SHADER,
                                         "hello-gl.v.glsl"
                                         );


  if(g_resources.vertex_shader == 0)
    return 0;

  g_resources.fragment_shader = make_shader(
                                            GL_FRAGMENT_SHADER,
                                            "hello-gl.f.glsl"
                                            );
  if (g_resources.fragment_shader == 0)
    return 0;

  
  g_resources.program = make_program(
                                     g_resources.vertex_shader,
                                     g_resources.fragment_shader
                                     );
  if (g_resources.program == 0)
    return 0;

  g_resources.uniforms.fade_factor
    = glGetUniformLocation(g_resources.program, "fade_factor");
  g_resources.uniforms.textures[0]
    = glGetUniformLocation(g_resources.program, "textures[0]");
  g_resources.uniforms.textures[1]
    = glGetUniformLocation(g_resources.program, "textures[1]");
  
  g_resources.attributes.position
    = glGetAttribLocation(g_resources.program, "position");
  
  return 1;


}

static void update_fade_factor(void)
{
  int milliseconds = glutGet(GLUT_ELAPSED_TIME);
  g_resources.fade_factor = sinf((float) milliseconds * 0.001f) * 0.5f + 0.5f;
  glutPostRedisplay();
}

static void render(void)
{
  glUseProgram(g_resources.program);
  glUniform1f(g_resources.uniforms.fade_factor, g_resources.fade_factor);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, g_resources.textures[0]);

  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, g_resources.textures[1]);
  glUniform1i(g_resources.uniforms.textures[1], 1);
  glBindBuffer(GL_ARRAY_BUFFER, g_resources.vertex_buffer);
  glVertexAttribPointer(
                        g_resources.attributes.position,  /* attribute */
                        2,                                /* size */
                        GL_FLOAT,                         /* type */
                        GL_FALSE,                         /* normalized? */
                        sizeof(GLfloat)*2,                /* stride */
                        (void*)0                          /* array buffer offset */
                        );
  glEnableVertexAttribArray(g_resources.attributes.position);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, g_resources.element_buffer);
  glDrawElements(
                 GL_TRIANGLE_STRIP,  /* mode */
                 4,                  /* count */
                 GL_UNSIGNED_SHORT,  /* type */
                 (void*)0            /* element array buffer offset */
                 );
  
  glDisableVertexAttribArray(g_resources.attributes.position);
  
  glutSwapBuffers();
    
  fprintf(stderr, "yo");
}


int main(int argc, char** argv)
{
  printf("Hello\n");
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
  glutInitWindowSize(400,300);
  glutCreateWindow("Hello World\n");

  glewInit();
  printf("GLEW_VERSION_2_0: %d\n",GLEW_VERSION_2_0);
  if(!GLEW_VERSION_2_0){
    printf("GLEW 2.0 unavailable\n");
  }

  if(!make_resources()){
    fprintf(stderr, "failed to load resources\n");
    return 1;
  } else {
    fprintf(stderr, "loaded resources\n");
  }

  glutDisplayFunc(&render);
  glutIdleFunc(&update_fade_factor);


  glutMainLoop();
  printf("Goodbye\n");
  return 0;
}
