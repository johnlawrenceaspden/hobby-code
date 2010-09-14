/*
 * AssociativeArray2D.java
 */

package com.aspden.widgets;

import java.util.*;

public class AssociativeArray2D extends Object {
    private Map stuff=new HashMap();

    public void add(String row, String column, Object o)
    {
        Object a=stuff.get(row);
        if(a==null)
        {
            a=new HashMap();
            stuff.put(row,a);
        }
        Map m=(Map)a;
        m.put(column,o);
   }

    public String[] getRows()
    {
        Set rows=stuff.keySet();
        return (String[]) rows.toArray(new String[0]);
    }

    public String[] getColumns()
    {
        Map columns=new HashMap();
        Map[] maps=(Map[])(stuff.values()).toArray(new Map[0]);
        for(int i=0; i<maps.length; i++) columns.putAll(maps[i]);
        return (String[]) (columns.keySet()).toArray(new String[0]);
    }
    
    public Object get(String row, String column)
    {
        Object a=stuff.get(row);
        if(a==null) return null;
        else{
            Map m=(Map)a;
            return m.get(column);
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main (String args[]) {
        AssociativeArray2D a=new AssociativeArray2D();
        a.add("frog","prince", new Integer(3));
        a.add("car","bus", new Double(2));
        a.add("frog", "toad", "aardvark");
        String[] r=a.getRows();
        String[] c=a.getColumns();
        System.out.print("    ");
        for(int j=0; j<c.length; j++){
            System.out.print(c[j]+" ");
        }
        System.out.println();
        for(int i=0; i<r.length; i++){
            System.out.print(r[i]+":");
            for(int j=0; j<c.length; j++){
                Object o=a.get(r[i], c[j]);
                if(o!=null){
                    System.out.print(o+" ");
                }
                else
                {
                    System.out.print("----"+" ");
                }
            }
            System.out.println();
        }


    }

}