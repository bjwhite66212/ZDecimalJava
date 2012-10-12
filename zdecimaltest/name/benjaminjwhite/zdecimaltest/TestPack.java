/*
 * TestPack.java - System Z decimal utility test
 * 
 * Copyright 2007 (c) Benjamin White
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
 * $Date: 2007/08/05 23:20:00 $
 * $Revision: 1.7 $
 * 
 */
package name.benjaminjwhite.zdecimaltest;

import org.junit.*;
import name.benjaminjwhite.zdecimal.*;

/**
 * Testing Script for name.benjaminjwhite.zdeimal.Packdec
 * 
 * @author zdecimal [ at ] benjaminjwhite.name
 * 
 */
public class TestPack {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		return;
	}

	@Test
	public void bytesTHex1() {
		final byte byt1[] = { (byte) 0x10, (byte) 0x15, (byte) 0x00,
				(byte) 0xff };
		Assert.assertEquals(PackDec.bytesToHex(byt1), "101500FF");
	}

	@Test
	public void longToPack1() {
		final long lnum1 = 5;
		final byte byta1[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				(byte) 0x5C };
		Assert.assertArrayEquals(PackDec.longToPack(lnum1), byta1);
	}

	@Test
	public void longToPack2() {
		final long lnum1 = -5;
		final byte byta1[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				(byte) 0x5D };
		Assert.assertArrayEquals(PackDec.longToPack(lnum1), byta1);
	}

	@Test
	public void longToPack3() {
		final long lnum1 = 0;
		final byte byta1[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				(byte) 0x0C };
		Assert.assertArrayEquals(PackDec.longToPack(lnum1), byta1);
	}

	@Test
	public void longToPack4() {
		final long lnum1 = 1234567890123456789l;
		final byte byta1[] = { 0, 0, 0, 0, 0, 0, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x90, (byte) 0x12,
				(byte) 0x34, (byte) 0x56, (byte) 0x78, (byte) 0x9c };
		Assert.assertArrayEquals(PackDec.longToPack(lnum1), byta1);
	}

	@Test
	public void longToPack5() {
		final long lnum1 = -1234567890123456789l;
		final byte byta1[] = { 0, 0, 0, 0, 0, 0, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x90, (byte) 0x12,
				(byte) 0x34, (byte) 0x56, (byte) 0x78, (byte) 0x9d };
		Assert.assertArrayEquals(PackDec.longToPack(lnum1), byta1);
	}

	@Test
	public void longToPack6() throws Exception {
		final long lnum1 = -1234567890123456789l;
		final byte byta1[] = { 0, 0, 0, 0, 0, 0, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x90, (byte) 0x12,
				(byte) 0x34, (byte) 0x56, (byte) 0x78, (byte) 0x9d };
		byte byta2[] = new byte[16];
		PackDec.longToPack(lnum1, byta2, 0, 16);
		Assert.assertArrayEquals(byta1, byta2);
	}

	@Test(expected = DecimalOverflowException.class)
	public void longToPack7() throws Exception {
		final long lnum1 = -1234567890123456789l;
		final byte byta1[] = { 0, 0, 0, 0, 0, 0, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x90, (byte) 0x12,
				(byte) 0x34, (byte) 0x56, (byte) 0x78, (byte) 0x9d };
		byte byta2[] = new byte[16];
		PackDec.longToPack(lnum1, byta2, 0, 5);
		Assert.assertArrayEquals(byta1, byta2);
	}

	@Test
	public void longToPack8() throws Exception {
		final long lnum1 = -1234567890123456789l;
		final byte byta1[] = { 0, 0, (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9d, 0, 0 };
		byte byta2[] = new byte[14];
		PackDec.longToPack(lnum1, byta2, 2, 10);
		Assert.assertArrayEquals(byta1, byta2);
	}

	@Test
	public void packToLong1() throws Exception {
		final byte byta1[] = { 0, 0, (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9d };
		Assert.assertEquals(-1234567890123456789l, PackDec.packToLong(byta1));
	}

	@Test
	public void packToLong2() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9c };
		Assert.assertEquals(1234567890123456789l, PackDec.packToLong(byta1));
	}

	@Test(expected = DataException.class)
	public void packToLong3() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x95 };
		Assert.assertEquals(1234567890123456789l, PackDec.packToLong(byta1));
	}

	@Test(expected = DataException.class)
	public void packToLong4() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0xa6,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9c };
		Assert.assertEquals(1234567890123456789l, PackDec.packToLong(byta1));
	}

	@Test(expected = DataException.class)
	public void packToLong4a() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x5a,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9c };
		Assert.assertEquals(1234567890123456789l, PackDec.packToLong(byta1));
	}

	@Test(expected = FixedPointDivideException.class)
	public void packToLong5() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x90, (byte) 0x12 };
		Assert.assertEquals(1234567890123456789l, PackDec.packToLong(byta1));
	}

	@Test(expected = FixedPointDivideException.class)
	public void packToLong5a() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x90, (byte) 0xdc };
		Assert.assertEquals(1234567890123456789l, PackDec.packToLong(byta1));
	}

	@Test
	public void packToLong6() throws Exception {
		final byte byta1[] = { (byte) 0x5c };
		Assert.assertEquals(5l, PackDec.packToLong(byta1));
	}

	@Test
	public void packToLong7() throws Exception {
		final byte byta1[] = { (byte) 0x5d };
		Assert.assertEquals(-5l, PackDec.packToLong(byta1));
	}

	@Test
	public void packToLong8() throws Exception {
		final byte byta1[] = { (byte) 0x5b };
		Assert.assertEquals(-5l, PackDec.packToLong(byta1));
	}

	@Test
	public void packToLong9() throws Exception {
		final byte byta1[] = { (byte) 0x0b };
		Assert.assertEquals(0l, PackDec.packToLong(byta1));
	}

	@Test
	public void packToLong10() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9c };
		Assert.assertEquals(1234567890123456789l, PackDec.packToLong(byta1, 0,
				10));
	}

	@Test
	public void packToLong11() throws Exception {
		final byte byta1[] = { 0, 0, (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9c, 0 };
		Assert.assertEquals(1234567890123456789l, PackDec.packToLong(byta1, 2,
				10));
	}
	
	@Test(expected = FixedPointDivideException.class)
	public void packToLong12() throws Exception {
		final byte bytea1[] = { 0x01, (byte) 0x84, 0x46, 0x74, 0x40, 0x73, 0x70,
				(byte) 0x95, 0x51, 0x62, 0x0c};
		Assert.assertEquals(4l, PackDec.packToLong(bytea1));
	}
	
	@Test
	public void packToLong13() throws Exception {
		// largest positive
		final byte bytea1[] = { (byte) 0x92, 0x23, 0x37, 0x20, 0x36, (byte) 0x85, 0x47, 0x75,
				(byte) 0x80, 0x7c  };
		Assert.assertEquals(9223372036854775807l,PackDec.packToLong(bytea1) );
	}

	@Test
	public void packToLong14() throws Exception {
		// largest negative
		final byte bytea1[] = { (byte) 0x92, 0x23, 0x37, 0x20, 0x36, (byte) 0x85, 0x47, 0x75,
				(byte) 0x80, (byte) 0x8b  };
		Assert.assertEquals(-9223372036854775808l,PackDec.packToLong(bytea1) );
	}

	@Test
	public void packToString1() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9c };
		Assert.assertEquals("+1234567890123456789", PackDec
						.packToString(byta1));
	}

	@Test(expected = DataException.class)
	public void packToString2() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0xa4, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9c };
		Assert.assertEquals("+1234567890123456789", PackDec
						.packToString(byta1));
	}

	@Test(expected = DataException.class)
	public void packToString3() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x3a, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9c };
		Assert.assertEquals("+1234567890123456789", PackDec
						.packToString(byta1));
	}

	@Test(expected = DataException.class)
	public void packToString4() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0xac };
		Assert.assertEquals("+1234567890123456789", PackDec
						.packToString(byta1));
	}

	@Test(expected = DataException.class)
	public void packToString5() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0xac };
		Assert.assertEquals("+1234567890123456789", PackDec
						.packToString(byta1));
	}

	@Test
	public void packToString6() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9b };
		Assert.assertEquals("-1234567890123456789", PackDec
						.packToString(byta1));
	}

	@Test
	public void packToString7() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9d };
		Assert.assertEquals("-1234567890123456789", PackDec
						.packToString(byta1));
	}

	@Test
	public void packToString8() throws Exception {
		final byte byta1[] = { (byte) 0x1d };
		Assert.assertEquals("-1", PackDec.packToString(byta1));
	}

	@Test
	public void packToString9() throws Exception {
		final byte byta1[] = { (byte) 0x1f };
		Assert.assertEquals("+1", PackDec.packToString(byta1));
	}

	@Test(expected = DataException.class)
	public void packToString10() throws Exception {
		final byte byta1[] = { (byte) 0x19 };
		Assert.assertEquals("+1", PackDec.packToString(byta1));
	}

	@Test
	public void packToString11() throws Exception {
		final byte byta1[] = { (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9d };
		Assert.assertEquals("-1234567890123456789", PackDec.packToString(byta1,
				0, 10));
	}

	@Test
	public void packToString12() throws Exception {
		final byte byta1[] = { 0, 0, 0, (byte) 0x12, (byte) 0x34, (byte) 0x56,
				(byte) 0x78, (byte) 0x90, (byte) 0x12, (byte) 0x34,
				(byte) 0x56, (byte) 0x78, (byte) 0x9d, 0 };
		Assert.assertEquals("-1234567890123456789", PackDec.packToString(byta1,
				3, 10));
	}

	@Test
	public void packToString13() throws Exception {
		final byte byta1[] = { 0, (byte) 0x3f, 0 };
		Assert.assertEquals("+3", PackDec.packToString(byta1, 1, 1));
	}

	@Test
	public void packToString14() throws Exception {
		final byte byta1[] = { (byte) 0x3f, 0 };
		Assert.assertEquals("+3", PackDec.packToString(byta1, 0, 1));
	}
	@Test public void stringToPack1() throws Exception {
		final byte byta1[] = { 
				0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (byte) 0x1c };
		Assert.assertArrayEquals( byta1, PackDec.stringToPack("1"));
	}
	@Test public void stringToPack2() throws Exception {
		final byte byta1[] = { 
				0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (byte) 0x1d };
		Assert.assertArrayEquals( byta1, PackDec.stringToPack("-1"));
	}
	@Test(expected=DataException.class) public void stringToPack3() throws Exception {
		final byte byta1[] = { 
				0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (byte) 0x1d };
		Assert.assertArrayEquals( byta1, PackDec.stringToPack("-a"));
	}
	@Test(expected=DataException.class) public void stringToPack4() throws Exception {
		final byte byta1[] = { 
				0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (byte) 0x1d };
		Assert.assertArrayEquals( byta1, PackDec.stringToPack("-1a"));
	}
	@Test() public void stringToPack5() throws Exception {
		final byte byta1[] = { 0, 0, 0, 0, 0, 0,
				(byte) 0x12,
				(byte) 0x34,
				(byte) 0x56,
				(byte) 0x78,
				(byte) 0x90,
				(byte) 0x12,
				(byte) 0x34,
				(byte) 0x56,
				(byte) 0x78,
				(byte) 0x9c };
		Assert.assertArrayEquals( byta1,
				PackDec.stringToPack("1234567890123456789"));
	}
	@Test() public void stringToPack6() throws Exception {
		final byte byta1[] = {    
				(byte) 0x12,
				(byte) 0x34,
				(byte) 0x56,
				(byte) 0x78,
				(byte) 0x90,
				(byte) 0x12,
				(byte) 0x34,
				(byte) 0x56,
				(byte) 0x78,
				(byte) 0x90,
				(byte) 0x12,
				(byte) 0x34,
				(byte) 0x56,
				(byte) 0x78,
				(byte) 0x90,
				(byte) 0x1c,
				};
		byte byta2[] = PackDec.stringToPack("1234567890123456789012345678901");
		Assert.assertArrayEquals( byta1, byta2 );
		}
		@Test() public void stringToPack7() throws Exception {
			final byte byta1[] = {    
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x1d,
					};
			byte byta2[] = PackDec.stringToPack("-1234567890123456789012345678901");
			Assert.assertArrayEquals( byta1, byta2 );
	}
		@Test() public void stringToPack8() throws Exception {
			final byte byta1[] = {    
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x1d,
					};
			byte byta2[] = PackDec.stringToPack("-123456.7890123456,789012345678901");
			Assert.assertArrayEquals( byta1, byta2 );
	}
		@Test(expected=DecimalOverflowException.class) public void stringToPack9() throws Exception {
			final byte byta1[] = {    
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x1d,
					};
			byte byta2[] = PackDec.stringToPack("-123456.7890123456,7890123456789012");
			Assert.assertArrayEquals( byta1, byta2 );
	}
		@Test public void stringToPack10() throws Exception {
			final byte byta1[] = {    
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x1c,
					};
			byte byta2[] = new byte[16]; 
			PackDec.stringToPack("1234567890123456789012345678901",
					byta2, 0, 16 );
			Assert.assertArrayEquals( byta1, byta2 );
			}
		@Test public void stringToPack11() throws Exception {
			final byte byta1[] = {  0, 0,   
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x1c,
					};
			byte byta2[] = new byte[17]; 
			PackDec.stringToPack("34567890123456789012345678901",
					byta2, 2, 15 );
			Assert.assertArrayEquals( byta1, byta2 );
			}
		@Test(expected=DecimalOverflowException.class) public void stringToPack12() throws Exception {
			final byte byta1[] = {  0, 0,   
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x1c,
					};
			byte byta2[] = new byte[17]; 
			PackDec.stringToPack("1234567890123456789012345678901",
					byta2, 2, 15 );
			Assert.assertArrayEquals( byta1, byta2 );
			}
		@Test(expected=DecimalOverflowException.class) public void stringToPack13() throws Exception {
			final byte byta1[] = {  0, 0,   
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x12,
					(byte) 0x34,
					(byte) 0x56,
					(byte) 0x78,
					(byte) 0x90,
					(byte) 0x1c,
					};
			byte byta2[] = new byte[17]; 
			PackDec.stringToPack("1234567890123456789012345678901",
					byta2, 0, 17 );
			Assert.assertArrayEquals( byta1, byta2 );
			}
}
