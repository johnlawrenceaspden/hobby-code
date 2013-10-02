// Compile and run with
// javac efficiencyandprogress.java 
// java -server efficiencyandprogress


 
public class efficiencyandprogress {


    public static long microbenchmark()
    {
          final int N = 1000000; 
        
        int[] a = new int[N];
        int[] b = new int[N];
        int[] c = new int[N];

        int i, count;
        long sum=0;

        for (i=0; i< N; i++) {
            a[i]=i;
            b[i]=i;
            
        }
        for(count=0;count<1000; count++){
            for (i=0; i< N; i++) {
                c[i]=a[i]+b[i];
            }

            if(count/100*100==count) {
                System.out.println("count" + count);
            }

            for (i=0; i< N; i++) {
                sum+=a[i];
            }
        }
        
        return sum;
      
    }

    public static void main(String[] args) {

        System.out.println("sum=" + microbenchmark());

    }
}

// time java -server efficiencyandprogress 
// count0
// count100
// count200
// count300
// count400
// count500
// count600
// count700
// count800
// count900
// sum=499999500000000

// real	0m20.021s
// user	0m20.016s
// sys	0m0.144s
