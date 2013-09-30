// Compile and run with
// javac efficiencyandprogress.java 
// java efficiencyandprogress


 
public class efficiencyandprogress {


    public static void main(String[] args) {


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
        for(count=0;count<100000; count++){
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

        System.out.println("sum="+ sum);

    }
}

