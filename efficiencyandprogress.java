// Compile and run with
// javac efficiencyandprogress.java 
// java -server efficiencyandprogress


 
public class efficiencyandprogress {


    public static long microbenchmark()
    {
        final int N = 1000000; 
        
        int[] a = new int[N];
        int[] b = new int[N];

        int i, count;
        long sum=0;

        for (i=0; i< N; i++) {
            a[i]=i;
        }

        for(count=0;count<1000; count++){
            for (i=0; i< N; i++) {
                b[i]+=a[i];
            }

            for (i=0; i< N; i++) {
                sum+=b[i];
            }
        }
        
        return sum;
      
    }

    public static void main(String[] args) {

        System.out.println("sum=" + microbenchmark());

    }
}

// javac efficiencyandprogress.java 
// time java -server efficiencyandprogress
// sum=250249749750000000

// real	0m19.306s
// user	0m21.200s
// sys	0m11.804s
