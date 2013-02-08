public class Euler{
    public static void main (String[] args){
        double cpuspeed = 1.662;
        long its = 10000000;
        double t=0;
        double y=0;
        double h=1.0/its;

        long start = System.nanoTime();
        for(long i = 0; i < its; i++){
            y = y+h*(t-y);
            t = t+h;
        }
        long finish = System.nanoTime();

        System.out.println("y=" + y + " t=" +t); 
        System.out.println("cycles/iteration: " +  ((int) ((cpuspeed * (finish - start)) / its)));
    }
}
