import java.util.*;
import cern.jet.random.tdouble.*;
import cern.jet.random.tdouble.engine.*;
 
class Gibbs
{
 
    public static void main(String[] arg)
    {
        int N=50000;
        int thin=1000;
        DoubleRandomEngine rngEngine=new DoubleMersenneTwister(new Date());
        Normal rngN=new Normal(0.0,1.0,rngEngine);
        Gamma rngG=new Gamma(1.0,1.0,rngEngine);
        double x=0;
        double y=0;
        System.out.println("Iter x y");
        for (int i=0;i<N;i++) {
            for (int j=0;j<thin;j++) {
                x=rngG.nextDouble(3.0,y*y+4);
                y=rngN.nextDouble(1.0/(x+1),1.0/Math.sqrt(2*x+2));
            }
            if (0 == (i % 1000)){
                System.out.println(i+" "+x+" "+y);
            }
        }
    }
}
