/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
// package ea.lenguajes;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.BitSet;
import java.util.Locale;
import java.util.Random;

/**
 *
 * @author Antonio Fernández Ares <antares@ugr.es>
 */
public class EaLenguajes {

    /**
     * @param args the command line arguments
     *
     */
    private static final int ITERATIONS = 100000;
    private static final int MAXLENGTH = 32768;

    private static DecimalFormat df = new DecimalFormat();

    public static void mutationBitflip() {

        Random r = new Random();

        for (int length = 16; length <= MAXLENGTH; length = length * 2) {
	    long timestart = System.nanoTime();
            BitSet b = new BitSet(length);

            for (int ite = 0; ite < ITERATIONS; ite++) {
                b.flip(r.nextInt(length));
            }

	    b.clear();
            long timeend = System.nanoTime();

            long estimated = timeend - timestart;

            System.out.println("java-bitflip," + length + " , " + (double) estimated / 1000000000);


        }
    }

    //TO DO
    public static void onemax() {

        Random r = new Random();

        int a;

        for (int length = 16; length <= MAXLENGTH; length = length * 2) {
	    long timestart = System.nanoTime();
            BitSet b = new BitSet(length);
            for (int ite = 0; ite < ITERATIONS; ite++) {
                a = b.cardinality();
            }

	    b.clear();
            long timeend = System.nanoTime();

            long estimated = timeend - timestart;

            System.out.println("java-onemax," + length + " , " + df.format((double) estimated / 1000000000));

        }
    }

    //TO DO
    public static void xover() {

        Random r = new Random();

        for (int length = 16; length <= MAXLENGTH; length = length * 2) {

	    long timestart = System.nanoTime();
            BitSet b1 = new BitSet(length);
            BitSet b2 = new BitSet(length);

            for (int ite = 0; ite < ITERATIONS; ite++) {
		for (int i = 0; i < length; i++) {
		    b1.set(i, r.nextBoolean());
		    b2.set(i, r.nextBoolean());
		}
		
		int initial_point = r.nextInt(length);
		int final_point = r.nextInt(length);
		if (initial_point > final_point) {
		    int t = initial_point;
		    initial_point = final_point;
		    final_point = t;
		}
		
		BitSet temp1 = (BitSet) b1.clone();
		BitSet temp2 = (BitSet) b2.clone();
		
		b1.clear(0, initial_point);
		b2.clear(initial_point, length);
		
		b1.or(temp2);
		b2.or(temp1);
	    }

	    long timeend = System.nanoTime();
	    
	    long estimated = timeend - timestart;

            System.out.println("java-xover," + length + " , " + (double) estimated / 1000000000);

        }
    }


    public static void griewank() {

        Random r = new Random();

        int sum;
        int prod;

        for (int length = 16; length <= MAXLENGTH; length = length * 2) {
	    long timestart = System.nanoTime();
            BitSet b = new BitSet(length);
            sum = 0;
            prod = 1;

            for (int ite = 1; ite <= b.length(); ite++) {
                int value = b.get(ite-1)? 1 : 0;
                sum+=(value) /4000;
                prod*=Math.cos(value/Math.sqrt(ite));
            }
	    b.clear();
            long timeend = System.nanoTime();
            long estimated = timeend - timestart;
            System.out.println("java," + length + " , " + df.format((double) estimated / 1000000000));

        }
    }




    public static void main(String[] args) {

        df.setMaximumFractionDigits(12);
        df.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.ENGLISH) );

        mutationBitflip();

        onemax();

        xover();

        griewank();

    }

}
