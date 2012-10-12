/*
 * PackDec.java - System Z decimal utility
 * 
 * Copyright 2006 (c) Benjamin White
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * $Author: bjwhite66212 $
 * $Date: 2007/09/25 00:05:28 $
 * $Revision: 1.10 $
 *
 */
package name.benjaminjwhite.zdecimal;

/**
 * 
 * Utility class to handle IBM System Z packed decimal data in Java. All packed
 * number operands must be valid format with valid sign. The longest packed
 * decimal number handled is 31 digits or 16 byted. Java long will not hold the
 * maximum packed decimal number. java.math.bigdecimal can be used to handle
 * larger numbers than will fit in java long.
 * 
 * "Decimal packed" format is a method of representing numbers in the IBM
 * System Z computers. It was also present in the 360, 370 and 390 series.
 * Decimal digits are stored as 4 bits, 0 through 9, two digits per byte. The
 * last four bits of a number are reserved for a sign. Positive are binary 1010,
 * 1100 1110 and 1111. Negative is 1011 and 1101. For example the number -354
 * woud be stored as 0x354d, 7251 would be stored 0x07251c.
 * 
 * COBOL is a popular mainframe language that has a number format with a "USAGE"
 * of COMPUTATIONAL-3 or CCOMP-3. COMP-3 is stored as packed decimal. An
 * example: <code><pre>
 *   01 PART-NUMBER.
 *      05  PART-NAME                   PIC X(20).
 *      05  PART-NUMBER                 PIC 9(5) USAGE IS COMP-3.
 *      05  PART-COST                   PIC 9(5)V99 USAGE IS COMP-3.
 *      05  FILLER                      PIC X(10).
 * </pre></code> The PART-NUMBER would be stored in memory as packed decimal and
 * occupy 3 bytes. The PART-COST would use 4 bytes. The implied decimal does not
 * reserve any storage
 * 
 * <br/>
 * Sysem Z, 360, 370 and 390 is a trademark of IBM Corporation
 * 
 * <br/>
 * Thanks for optimization suggestions: <a href="nntp://comp.lang.java.programmer">comp.lang.java.programmer</a>
 * <br/>
 * <a href="http://www.telekinesis.com.au">Esmond Pitt</a>
 * <br/>
 * "Barak Shilo" for alert to decimal overflow bug in PackDec.<br/>
 * 
 * 
 * @see <a href="http://www.ibm.com">IBM.COM</a>
 * 
 * @see <a href="http://benjaminjwhite.name/zdecimal">Z Decimal for java project
 *      home page</a>
 * 
 * @author zdecimal [ at ] benjaminjwhite.name
 * @version 4
 */

public class PackDec {

	static final char[] digits = { '0', '1', '2', '3', '4', '5', '6', '7', '8',
			'9', 'A', 'B', 'C', 'D', 'E', 'F' };

	/**
	 * Hex values of Byte array
	 * 
	 * @param bytes
	 *            array of bytes
	 * @return Hex string of input
	 */
	public static String bytesToHex(byte[] bytes) {
		int byteslen = bytes.length;
		char chars[] = new char[bytes.length * 2];
		int i;
		int j = 0;
		for (i = 0; i < byteslen; i++) {
			byte b1 = bytes[i];
			chars[j++] = digits[(b1 & 0xf0) >>> 4];
			chars[j++] = digits[b1 & 0x0f];
		}
		return (new String(chars));
	}

	/**
	 * 
	 * Convert "long" to byte array 16 long of packed decimal number
	 * 
	 * @param lnum
	 *            long number to convert
	 * @return byte array 16 long
	 */
	public static byte[] longToPack(long lnum) {
		byte[] pknum = new byte[16]; // max possible size
		int i;
		int pknumlen = pknum.length;
		long longwork;
		i = pknumlen - 1;
		longwork = lnum;
		if (longwork < 0) {
			pknum[i] = 13; // 0x0d negative sign
			longwork *= -1;
		} else
			pknum[i] = 12; // 0x0c positive sign
		pknum[i] = (byte) (pknum[i] | ((longwork % 10) << 4));
		longwork /= 10;
		--i;
		while (0 != longwork) {
			pknum[i] = (byte) (longwork % 10);
			longwork /= 10;
			pknum[i] = (byte) (pknum[i] | ((longwork % 10) << 4));
			longwork /= 10;
			--i;
		}
		return (pknum);
	}

	/**
	 * Convenience method to convert a long to packed decimal. The packed number
	 * is stored in an existing array.
	 * 
	 * @param lnum
	 *            Number to be converted
	 * @param bytearray
	 *            Contains result
	 * @param offset
	 *            Location in array
	 * @param len
	 *            Number of bytes of result
	 * @throws DecimalOverflowException
	 *             If result is larger than result length
	 */
	public static void longToPack(long lnum, byte[] bytearray, int offset,
			int len) throws DecimalOverflowException {
		byte[] pknum = longToPack(lnum); // get packed num of length 16
		int pklen = pknum.length;
		if (len < pklen) // check to see of number too big
			if (pknum[pklen - len - 1] != 0)
				throw new DecimalOverflowException("Number too big");
		int i = offset + len - 1;
		int j = pklen - 1;
		int k;
		for (k = 0; k < len; k++) {
			bytearray[i] = pknum[j];
			--i;
			--j;
		}
	}

	/**
	 * Convert a byte array containing a packed dicimal number to "long"
	 * 
	 * @param pknum
	 *            byte array of 1 to 16
	 * @return long number
	 * @exception DataException
	 *                input is not packed decimal format
	 * @exception FixedPointDivideException
	 *                number would not fit in long
	 */
	public static long packToLong(byte[] pknum) throws DataException,
			FixedPointDivideException {
		// final long maxlong   = 9223372036854775807l; // largest long positive
		final long maxlongm7 = 9223372036854775800l; // largest long minus 7
		final long maxlongm8 = 9223372036854775800l; // largest negative long minus 8
		final long maxlongdiv10 = 922337203685477580l; // largest long / 10
		int lenpknum = pknum.length; // input length
		long lnum = 0; // initialize
		int endloop; // loop counter
		int i; // subscript
		int nibble; // four bits
		int nibble2; // last four bits
		endloop = lenpknum - 1; // don't include last byte in loop
		for (i = 0; i < endloop; i++) {
			if (lnum > maxlongdiv10)
				throw new FixedPointDivideException("Number too big "
						+ bytesToHex(pknum));
			lnum *= 10;
			nibble = (pknum[i] & 0xf0) >>> 4;
			if (nibble > 9)
				throw new DataException("Invalid decimal digit: " + nibble
						+ " in " + bytesToHex(pknum));
			lnum += nibble;
			if (lnum > maxlongdiv10)
				throw new FixedPointDivideException("Number too big "
						+ bytesToHex(pknum));
			lnum *= 10;
			nibble = (pknum[i] & 0x0f);
			if (nibble > 9)
				throw new DataException("Invalid decimal digit: " + nibble
						+ " in " + bytesToHex(pknum));
			lnum += nibble;
		}
		// Process the last byte. Lower 4 bits are sign
		if (lnum > maxlongdiv10)
			throw new FixedPointDivideException("Number too big "
					+ bytesToHex(pknum));
		lnum *= 10;
		nibble = (pknum[i] & 0xf0) >>> 4;
		nibble2 = (pknum[i] & 0x0f);
		if (nibble2 < 10)
			throw new DataException("Invalid deciaml sign  "
					+ bytesToHex(pknum));
		if (nibble > 9)
			throw new DataException("Invalid decimal digit: " + nibble + " in "
					+ bytesToHex(pknum));
		if ( nibble2 == 11 || nibble2 == 13 )
			if (lnum > maxlongm8)
				throw new FixedPointDivideException("Number too big "
					+ bytesToHex(pknum));
			else {
				lnum *= -1;
				lnum -= nibble;
			}
		else
			if (lnum > maxlongm7)
				throw new FixedPointDivideException("Number too big "
					+ bytesToHex(pknum));
			else
				lnum += nibble;
		return (lnum);
	}

	/**
	 * Selects a packed decimal number from a byte array, then converts to a
	 * Long value of the number.
	 * 
	 * @param bytearray
	 *            contains packed number
	 * @param offset
	 *            into byte array
	 * @param len
	 *            length of packed field
	 * @return Long value of byte array of packed decimal
	 * @throws DataException
	 * @throws FixedPointDivideException
	 */
	public static long packToLong(byte[] bytearray, int offset, int len)
			throws DataException, FixedPointDivideException {
		byte[] pknum = new byte[len];
		int i = offset;
		int j;
		for (j = 0; j < len; j++) {
			pknum[j] = bytearray[i];
			i++;
		}
		return (packToLong(pknum));
	}

	/**
	 * Convert a byte array containing a packed dicimal number to String value
	 * 
	 * @param pknum
	 *            byte array containing packed number, 1 to 16 long
	 * @return String of packed number. First byte is sign
	 * @exception DataException
	 *                input is not packed decimal format
	 */
	public static String packToString(byte[] pknum) throws DataException {
		StringBuffer strbuf = new StringBuffer(32);
		int endloop; // loop counter
		int i; // subscript
		int nibble; // four bits
		endloop = pknum.length - 1; // Don't include last byte
		for (i = 0; i < endloop; i++) {
			nibble = (pknum[i] & 0xf0) >>> 4;
			if (nibble > 9)
				throw new DataException("Invalid decimal digit: " + nibble
						+ " in " + bytesToHex(pknum));
			strbuf.append(digits[nibble]);
			nibble = (pknum[i] & 0x0f);
			if (nibble > 9)
				throw new DataException("Invalid decimal digit: " + nibble
						+ " in " + bytesToHex(pknum));
			strbuf.append(digits[nibble]);
		}
		// Last byte contains sign
		nibble = (pknum[i] & 0xf0) >>> 4;
		if (nibble > 9)
			throw new DataException("Invalid decimal digit: " + nibble + " in "
					+ bytesToHex(pknum));
		strbuf.append(digits[nibble]);
		nibble = (pknum[i] & 0x0f);
		if (nibble < 10)
			throw new DataException("Invalid deciaml sign: "
					+ bytesToHex(pknum));
		if (11 == nibble || 13 == nibble)
			strbuf.insert(0, '-');
		else
			strbuf.insert(0, '+');
		return (strbuf.toString());
	}

	/**
	 * Selects a packed decimal number from a byte array, then converts to a
	 * String value of the number.
	 * 
	 * 
	 * @param bytearray
	 *            that contains a packed number and possibly other data
	 * @param offset
	 *            to the packed nubmer to be converted
	 * @param len
	 *            number of bytes in length of the packed number
	 * @return String value of packed number. First character is sign
	 * @throws DataException
	 *             selected array is not in packed decimal format
	 */

	public static String packToString(byte[] bytearray, int offset, int len)
			throws DataException, FixedPointDivideException {
		byte[] pknum = new byte[len];
		int i = offset;
		int j;
		for (j = 0; j < len; j++) {
			pknum[j] = bytearray[i];
			i++;
		}
		return (packToString(pknum));
	}

	/**
	 * Converts String to packed decimal number. Decimal points, commas and
	 * spaces are ignored. Sign character is processed. Avoid multiple signs.
	 * Characters other than digits are invalid and will cause DataException.
	 * Comma, blank, period, dollar sign and plus are ignored. Scaling and
	 * exponents are not valid.
	 * 
	 * @param str
	 *            String of number to convert
	 * @return byte array of packed decimal, 16 long
	 * @throws DataException
	 *             Invalid characters in input string
	 * @throws DecimalOverflowException
	 *             Too many digits in input string
	 */
	public static byte[] stringToPack(String str) throws DataException,
			DecimalOverflowException {
		int i; // string index
		int j; // byte array index
		boolean nibble_ordinal = false;
		char ch1;
		byte nibble;
		byte[] pknum = new byte[16];
		i = str.length() - 1;
		j = 15; /* byte index */
		pknum[j] = 12; // start with positive sign
		while (i > -1) {
			ch1 = str.charAt(i);
			if ('0' <= ch1 && '9' >= ch1) {
				if (j < 0) { /*
							 * rarely will be need, may remove for performance
							 */
					throw new DecimalOverflowException("Number too large:"
							+ str);
				}
				nibble = (byte) (ch1 - '0');
				if (nibble_ordinal) {
					pknum[j] = (byte) (pknum[j] | nibble);
					nibble_ordinal ^= true;
				} else {
					pknum[j] = (byte) (pknum[j] | nibble << 4);
					nibble_ordinal ^= true;
					--j;
				}
				--i; // get next char
			} else {
				switch (ch1) {
				case ',':
				case ' ':
				case '.':
				case '$':
				case '+':
					--i; // get next char
					break;
				case '-':
					pknum[15] = (byte) (pknum[15] & 0xf0);
					pknum[15] = (byte) (pknum[15] | 0x0d);
					--i; // get next char
					break;
				default:
					throw new DataException("Invalid decimal digit: " + ch1
							+ " from input: " + str);
				}
			}
		}
		return (pknum);
	}

	/**
	 * Convenience method to convert a String to packed decimal. The packed
	 * number is stored in an existing array.
	 * 
	 * @param str
	 *            Number to be converted
	 * @param bytearray
	 *            Contains result
	 * @param offset
	 *            Location in array
	 * @param len
	 *            Number of bytes of result
	 * @throws DecimalOverflowException
	 *             If packed number is too big to fit in the target length.
	 */
	public static void stringToPack(String str, byte[] bytearray, int offset,
			int len) throws DecimalOverflowException, DataException {
		byte[] pknum = stringToPack(str); /* get packed num of length 16 */
		int pklen = pknum.length;
		if (len > pklen) // target field can't be over 16 bytes
			throw new DecimalOverflowException(
					"Length of target field greater than 16: " + len);
		if (len < pklen) // check to see of number too big
			if (pknum[pklen - len - 1] != 0)
				throw new DecimalOverflowException("Number too big: "
						+ PackDec.bytesToHex(pknum));
		int i = offset + len - 1;
		int j = pklen - 1;
		int k;
		for (k = 0; k < len; k++) {
			bytearray[i] = pknum[j];
			--i;
			--j;
		}
	}
}
