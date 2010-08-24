import java.io.*;  
   
public class FileLeak  
{  
    public static void main(String[] args)  
    {  
        int count = 0;  
        FileInputStream[] file = new FileInputStream[50000];  
   
        try  
        {  
            while ( count < 50000 )  
                file[count++] = new FileInputStream("FileLeak.java");  
        }  
        catch (Exception ex)  
        {  
            ex.printStackTrace();  
        }  
        finally  
        {  
            System.out.println(count);  
            try{Thread.sleep(10 * 1000);}catch(Exception ex2){;}  
        }  
    }  
} 