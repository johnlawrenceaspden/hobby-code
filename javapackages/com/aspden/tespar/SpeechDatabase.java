/*
 * SpeechDatabase.java
 */

package com.aspden.tespar;

import java.io.*;
import java.text.*;
import com.aspden.*;

/**
 *Provides information about layout on speech databases.
 */
public class SpeechDatabase extends Object {
    private String name;
    private String[] filenames;
    private String[] sounds;
    private String[] cutenames;
    private String rootdirectory;
    private String[] records;
    private int frequency;
    private static final String speechroot="C:\\speech databases";
    
     
    private void constructorCommonCode(String rootdirectory, String[] records, String[][] names, String name, int frequency )
    {
        this.name=name;
        this.rootdirectory=rootdirectory;
        this.filenames=names[0];
        this.sounds=names[1];
        this.cutenames=names[2];
        this.records=records;
        this.frequency=frequency;
        if(filenames.length!=cutenames.length) throw new IllegalArgumentException("Cute Names and Filenames Out of Sync");
        if(sounds.length!=cutenames.length) throw new IllegalArgumentException("Cute Names and Sounds Out of Sync");
    }

    /** Creates new SpeechDatabase */
    public SpeechDatabase(String rootdirectory, int records, String[][] names, String name, int frequency ) {
        String[] r=new String[records];
        DecimalFormat f=new DecimalFormat("00");
        for(int i=0; i<r.length; i++){
            r[i]="record"+f.format(i);
        }
        constructorCommonCode(rootdirectory, r, names, name, frequency );
    }
    
    /** Creates new SpeechDatabase */
    public SpeechDatabase(String rootdirectory, int[] records, String[][] names, String name, int frequency ) {
        String[] r=new String[records.length];
        DecimalFormat f=new DecimalFormat("00");
        for(int i=0; i<r.length; i++){
            r[i]="record"+f.format(records[i]);
        }
        constructorCommonCode(rootdirectory, r, names, name, frequency);
    }
    
    public SpeechDatabase firstFiveGrabs()
    {
        return new SpeechDatabase(rootdirectory, 5, new String[][]{filenames, sounds, cutenames}, name, frequency);
    }

    public String getName()
    {
        return name;
    }
    
    public int getFrequency()
    {
        return frequency;
    }
    
    public String getRoot()
    {
        return speechroot+java.io.File.separator+rootdirectory;
    }
    
    public String[] getSubdirs()
    {
        return records;
    }
    
    public String[] getFiles()
    {
        return filenames;
    }
    
    public String[] getCuteNames()
    {
        return cutenames;
    }
    
    public String[] getWords()
    {
        return sounds;
    }
    
    private static final String[] digitfiles={"o.txt","z.txt","1.txt","2.txt","3.txt","4.txt","5.txt","6.txt","7.txt","8.txt","9.txt"};
    private static final String[] digitsounds={"oh", "zero", "one", "two", "three","four","five", "six", "seven", "eight", "nine"};
    private static final String[] cutedigits={"O","0","1","2","3","4","5","6","7","8","9"};
    private static final String[][] digits ={digitfiles, digitsounds, cutedigits};
    
    private static final String[] menufiles={"nextlevel.txt","messages.txt","callregister.txt","profile.txt","settings.txt","calldivert.txt","games.txt","calculator.txt","infrared.txt","calendar.txt"};
    private static final String[] menusounds={"next level","messages","callregister","profile","settings","calldivert","games","calculator","infrared","calendar"};
    private static final String[][] menu = {menufiles, menusounds, menusounds};
    
    private static final String[] natofiles={"Alpha.txt", "Bravo.txt", "Charlie.txt", "Delta.txt", "Echo.txt", "Foxtrot.txt", "Golf.txt", "Hotel.txt", "India.txt", "Juliet.txt", "Kilo.txt", "Lima.txt", "Mike.txt", "November.txt", "Oscar.txt", "Papa.txt", "Quebec.txt", "Romeo.txt",  "Sierra.txt", "Tango.txt", "Uniform.txt", "Victor.txt", "Whiskey.txt", "Xray.txt", "Yankee.txt", "Zulu.txt"};
    private static final String[] natosounds={"Alpha", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot", "Golf", "Hotel", "India", "Juliet", "Kilo", "Lima", "Mike", "November", "Oscar", "Papa", "Quebec", "Romeo",  "Sierra", "Tango", "Uniform", "Victor", "Whiskey", "Xray", "Yankee", "Zulu"};
    private static final String[] cutenato={"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r",  "s", "t", "u", "v", "w", "x", "y", "z"};
    private static final String[][] nato = {natofiles, natosounds, cutenato};
    
    private static final String[] mixfiles=Stuff.concat(digitfiles, menufiles, natofiles );
    private static final String[] mixsounds=Stuff.concat(digitsounds, menusounds, natosounds );
    private static final String[] cutemix=Stuff.concat(cutedigits, menusounds, cutenato );
    private static final String[][] mix={mixfiles, mixsounds, cutemix };
    
    public static final SpeechDatabase JamesDigitsUniversity=new SpeechDatabase("jamesdigitsuniversity",23, digits, "James Christie's original digit recordings", 16000);
    public static final SpeechDatabase JohnDigitsHeadsetTooClose=new SpeechDatabase("johndigitsheadsettooclose",12, digits, "John Aspden digits with James' headset microphone but wearing it right in front of the mouth.", 16000);
    public static final SpeechDatabase JohnDigitsSony=new SpeechDatabase("johndigitssony",26, digits, "John Aspden digits with Sony mike", 16000);
    public static final SpeechDatabase JohnMenuHeadset=new SpeechDatabase("johnmenuheadset",10, menu, "John Aspden mobile menu with James' headset mike", 16000);
    public static final SpeechDatabase JohnMenuHeadsetTooClose=new SpeechDatabase("johnmenuheadsettooclose",10, menu, "John Aspden mobile menu with James' headset microphone but wearing it right in front of the mouth.", 16000);
    public static final SpeechDatabase JohnMenuSony=new SpeechDatabase("johnmenusony",21, menu, "John Aspden mobile menu with Sony mike", 16000);
    public static final SpeechDatabase JohnNatoSony=new SpeechDatabase("johnnatosony",6, nato, "John Aspden nato phonetic alphabet with Sony mike", 16000);
    public static final SpeechDatabase JohnNatoSony44100=new SpeechDatabase("johnnatosony44100",11, nato, "John Aspden nato phonetic alphabet with Sony mike 44100Hz", 44100);
    public static final SpeechDatabase LiveJohnDigitsSony= new SpeechDatabase("livejohndigitssony", 21, digits, "John Aspden live demo recordings Sony mike.", 16000); 
    public static final SpeechDatabase MoreLiveJohnDigitsSony= new SpeechDatabase("morelivejohndigitssony", 22, digits, "More John Aspden live demo recordings Sony mike.", 16000); 
    public static final SpeechDatabase MoreOguzDigitsSony11k=new SpeechDatabase("moreoguzdigitssony11k",14, digits, "Another set of Oguz Guceri digits with Sony mike at 11025", 11025);
    public static final SpeechDatabase OguzDigitsBhp=new SpeechDatabase("oguzdigitsbhp",10, digits, "Some Oguz digits (what is bhp?)", 16000);
    public static final SpeechDatabase OguzDigitsSony=new SpeechDatabase("oguzdigitssony",22, digits, "Oguz Guceri digits with Sony mike", 16000);
    public static final SpeechDatabase OguzDigitsSony11k=new SpeechDatabase("oguzdigitssony11k",5, digits, "Oguz Guceri digits with Sony mike at 11025", 11025);
    public static final SpeechDatabase OguzMenuHeadset=new SpeechDatabase("oguzmenuheadset",11, menu, "Oguz Guceri mobile menu with James' headset mike", 16000);
    public static final SpeechDatabase OguzMixSony= new SpeechDatabase("oguzmixsony", 22, mix, "Oguz mixed data (suspect contains files from other databases)", 16000); 
    public static final SpeechDatabase OguzNatoHeadsetGoodSep5=new SpeechDatabase("oguznatoheadsetgoodsep5", 11, nato, "Oguz Guceri nato alphabet headset", 16000);
    public static final SpeechDatabase OguzNatoHeadsetHighVol=new SpeechDatabase("oguznatoheadsethighvol", 5, nato, "Oguz Guceri nato alphabet headset", 16000);
    public static final SpeechDatabase OguzNatoHeadsetMidHigh=new SpeechDatabase("oguznatoheadsetmidhigh", 5, nato, "Oguz Guceri nato alphabet headset", 16000);
    public static final SpeechDatabase OguzNatoHeadsetMidVol=new SpeechDatabase("oguznatoheadsetmidvol", 5, nato, "Oguz Guceri nato alphabet headset", 16000);
    public static final SpeechDatabase OguzNatoHeadsetRandomSep5=new SpeechDatabase("oguznatoheadsetrandomsep5", 11, nato, "Oguz Guceri nato alphabet headset", 16000);
    public static final SpeechDatabase OguzNatoSony=new SpeechDatabase("oguznatosony",11, nato, "Oguz Guceri nato phonetic alphabet with Sony mike", 16000);

    public static final SpeechDatabase FavouriteJamesDigitsUniversity=new SpeechDatabase("jamesdigitsuniversity",new int[]{0,1,2,3,4,6,7,8}, digits, "First 8 of James Christie's original digit recordings without record05 which was dodgy", 16000);
    
    /*Small selections of files for use when testing*/
    private static final String[] testfiles={"o.txt","z.txt","1.txt"};
    private static final String[] testsounds={"oh", "zero", "one"};
    private static final String[] cutetest={"O","0","1"};
    private static final String[][] test={testfiles, testsounds, cutetest};
    
    public static final SpeechDatabase JamesTestDB=new SpeechDatabase("jamesdigitsuniversity",new int[]{0,1}, test, "JamesTest", 16000);
    public static final SpeechDatabase JohnTestDB=new SpeechDatabase("johndigitssony",new int[]{0,1,2}, test, "JohnTest", 16000);
    
    public static final String TestFile="C:\\speech databases\\jamesdigitsuniversity\\record00\\6.txt";
    public static final int TestFreq=16000;
}