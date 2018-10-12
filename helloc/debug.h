/* redirect to stderr */
void debug_vprintf(const char *fmt, ...);

/* Call as: (DOUBLE PARENTHESES ARE MANDATORY) */
/* var_debug(("outfd = %d, somefailed = %d\n", outfd, somefailed)); */
#define var_debug(x) do { if (DEBUG) { debug_vprintf ("%s:%d:%s(): ",   \
                                                      __FILE__,  __LINE__, __func__); debug_vprintf x; }} while (0)

/* var_dump("%s" variable_name); */
#define var_dump(fmt, var) do { if (DEBUG) { debug_vprintf ("%s:%d:%s(): ", \
                                                            __FILE__,  __LINE__, __func__); debug_vprintf ("%s = " fmt, #var, var); }} while (0)

#define DEBUG_HERE do { if (DEBUG) { debug_vprintf ("%s:%d:%s(): HERE\n", \
                                                    __FILE__,  __LINE__, __func__); }} while (0)


#define DS(symbol) var_dump("%s\n",symbol)
#define DI(symbol) var_dump("%i\n",symbol)

/* printf("%d\n", DBGI(1*2*3*4*5*6));  */
/* hello.c:86:main(): 1*2*3*4*5*6->720 */
/* 720                                 */
#define DBGI(expr) ({int   g2rE3Jo=expr; fprintf(stderr, "%s:%d:%s(): ""%s->%i\n", __FILE__,  __LINE__, __func__, #expr, g2rE3Jo); g2rE3Jo;})
#define DBGS(expr) ({char *g2rE3Jo=expr; fprintf(stderr, "%s:%d:%s(): ""%s->%s\n", __FILE__,  __LINE__, __func__, #expr, g2rE3Jo); g2rE3Jo;})
