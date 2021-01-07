int main(void) {
  printf("hello world!");
  printf("hello", "world!");
  if(du->fc == 2) printf(du->fc) else printf("%s", du.fc);
  if(du->fc == 2) {
    printf(du->fc);
  } else {

    printf("%s", du.fc);

    printf("%s\n", du->fc, du.fc);
  }
  return a->b->c.d->fc ;
  return a->b->c.d.fc ;
  return (a->b->c.d).fc ;
  return (a->b->c.d)->fc ;

}


