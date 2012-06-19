/*
 * Copyright 2012 Michael Ossmann
 *
 * This file is part of HackRF.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street,
 * Boston, MA 02110-1301, USA.
 */

#include <stdint.h>
#include <libopencm3/lpc43xx/ssp.h>
#include <libopencm3/lpc43xx/scu.h>
#include <libopencm3/lpc43xx/gpio.h>
#include "hackrf_core.h"
#include "rffc5071.h"

/* Set up pins for bit-banged serial interface. */
void rffc5071_init(void)
{
	/* Configure GPIO pins. */
	scu_pinmux(SCU_MIXER_ENX, SCU_GPIO_FAST);
	scu_pinmux(SCU_MIXER_SCLK, SCU_GPIO_FAST);
	scu_pinmux(SCU_MIXER_SDATA, SCU_GPIO_FAST);

	/* Set GPIO pins as outputs. */
	GPIO3_DIR |= (PIN_MIXER_ENX | PIN_MIXER_SCLK | PIN_MIXER_SDATA);

	/* set to known state */
	gpio_set(PORT_MIXER, PIN_MIXER_ENX); /* active low */
	gpio_clear(PORT_MIXER, (PIN_MIXER_SCLK | PIN_MIXER_SDATA));

	//FIXME hard coded setup, fields not broken out
	/* initial setup */
	rffc5071_reg_write(RFFC5071_P2_FREQ1, 0x0000);
	rffc5071_reg_write(RFFC5071_VCO_AUTO, 0xff00);
	rffc5071_reg_write(RFFC5071_CT_CAL1, 0xacbf);
	rffc5071_reg_write(RFFC5071_CT_CAL2, 0xacbf);
	rffc5071_reg_write(RFFC5071_TEST, 0x0005);

	/* set to be configured via 3-wire interface, not control pins */
	rffc5071_reg_write(RFFC5071_SDI_CTRL, 0x8000);

	//rffc5071_reg_write(MIX_CONT, 0xc800); /* full duplex */
	rffc5071_reg_write(RFFC5071_MIX_CONT, 0x4800); /* half duplex */

	/*
	 * setup for 250 MHz LO:
 	 * n_lo = 4
 	 * lodiv = 16 (2^4, so set to 4)
 	 * fvco = 4 GHz
 	 * fbkdiv = 4
 	 * ndiv = 20
 	 * n = 20
 	 * nummsb = 0
 	 * numlsb = 0
 	 */
	//rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x0a48);
	//rffc5071_reg_write(RFFC5071_P1_FREQ2, 0x0000);
	//rffc5071_reg_write(RFFC5071_P1_FREQ3, 0x0000);
	/* charge pump set for VCO > 3.2 GHz */
	//rffc5071_reg_write(RFFC5071_LF, 0xbefb);

	/*
	 * setup for 400 MHz LO:
 	 * lodiv = 8 (2^3, so set to 3)
 	 * fvco = 3200 MHz
 	 * fbkdiv = 2
 	 * ndiv = 32
 	 * n = 32
 	 * nummsb = 0
 	 * numlsb = 0
 	 */
	//rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x1034);
	//rffc5071_reg_write(RFFC5071_P1_FREQ2, 0x0000);
	//rffc5071_reg_write(RFFC5071_P1_FREQ3, 0x0000);

	/*
	 * setup for 500 MHz LO:
 	 * n_lo = 3
 	 * lodiv = 8 (2^3, so set to 3)
 	 * fvco = 4 GHz
 	 * fbkdiv = 4
 	 * ndiv = 20
 	 * n = 20
 	 * nummsb = 0
 	 * numlsb = 0
 	 */
	rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x0a39);
	rffc5071_reg_write(RFFC5071_P1_FREQ2, 0x0000);
	rffc5071_reg_write(RFFC5071_P1_FREQ3, 0x0000);
	/* charge pump set for VCO > 3.2 GHz */
	rffc5071_reg_write(RFFC5071_LF, 0xbefb);

	/*
	 * setup for 513 MHz LO:
 	 * n_lo = 3
 	 * lodiv = 8 (2^3, so set to 3)
 	 * fvco = 4104 MHz
 	 * fbkdiv = 4
 	 * ndiv = 20.52
 	 * n = 20
 	 * nummsb = 34078 (0x851e)
 	 * numlsb = 184 (0xb8)
 	 */
	//rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x0a38);
	//rffc5071_reg_write(RFFC5071_P1_FREQ2, 0x851e);
	//rffc5071_reg_write(RFFC5071_P1_FREQ3, 0xb800);
	/* charge pump set for VCO > 3.2 GHz */
	//rffc5071_reg_write(RFFC5071_LF, 0xbefb);

	/*
	 * setup for 1 GHz LO:
 	 * n_lo = 2
 	 * lodiv = 4 (2^2, so set to 2)
 	 * fvco = 4 GHz
 	 * fbkdiv = 4
 	 * ndiv = 20
 	 * n = 20
 	 * nummsb = 0
 	 * numlsb = 0
 	 */
	//rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x0a29);
	//rffc5071_reg_write(RFFC5071_P1_FREQ2, 0x0000);
	//rffc5071_reg_write(RFFC5071_P1_FREQ3, 0x0000);
	/* charge pump set for VCO > 3.2 GHz */
	//rffc5071_reg_write(RFFC5071_LF, 0xbefb);

	/*
	 * setup for 1417 MHz LO:
 	 * n_lo = 1
 	 * lodiv = 2 (2^1, so set to 1)
 	 * fvco = 2834 MHz
 	 * fbkdiv = 2
 	 * ndiv = 28.34
 	 * n = 28
 	 * nummsb = 22282 (570a)
 	 * numlsb = 61 (0x3d)
 	 */
	//rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x0e14);
	//rffc5071_reg_write(RFFC5071_P1_FREQ2, 0x570a);
	//rffc5071_reg_write(RFFC5071_P1_FREQ3, 0x3d00);
	/* charge pump set for VCO > 3.2 GHz */
	//rffc5071_reg_write(RFFC5071_LF, 0xbefb);

	/*
	 * setup for 2 GHz LO:
 	 * n_lo = 1
 	 * lodiv = 2 (2^1, so set to 1)
 	 * fvco = 4 GHz
 	 * fbkdiv = 4
 	 * ndiv = 20
 	 * n = 20
 	 * nummsb = 0
 	 * numlsb = 0
 	 */
	//rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x0a19);
	//rffc5071_reg_write(RFFC5071_P1_FREQ2, 0x0000);
	//rffc5071_reg_write(RFFC5071_P1_FREQ3, 0x0000);
	/* charge pump set for VCO > 3.2 GHz */
	//rffc5071_reg_write(RFFC5071_LF, 0xbefb);

	/*
	 * setup for 2191 GHz LO:
 	 * n_lo = 1
 	 * lodiv = 2 (2^1, so set to 1)
 	 * fvco = 4382 MHz
 	 * fbkdiv = 4
 	 * ndiv = 21.91
 	 * n = 21
 	 * nummsb = 59637 (0xe8f5)
 	 * numlsb = 194 (0xc2)
 	 */
	//rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x0a98);
	//rffc5071_reg_write(RFFC5071_P1_FREQ2, 0xe8f5);
	//rffc5071_reg_write(RFFC5071_P1_FREQ3, 0xc200);
	/* charge pump set for VCO > 3.2 GHz */
	//rffc5071_reg_write(RFFC5071_LF, 0xbefb);

	/*
	 * setup for 2341 GHz LO:
 	 * n_lo = 1
 	 * lodiv = 2 (2^1, so set to 1)
 	 * fvco = 4682 MHz
 	 * fbkdiv = 4
 	 * ndiv = 23.41
 	 * n = 23
 	 * nummsb = 26869 (0x68f5)
 	 * numlsb = 194 (0xc2)
 	 */
	//rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x0b9a);
	//rffc5071_reg_write(RFFC5071_P1_FREQ2, 0x68f5);
	//rffc5071_reg_write(RFFC5071_P1_FREQ3, 0xc200);
	/* charge pump set for VCO > 3.2 GHz */
	//rffc5071_reg_write(RFFC5071_LF, 0xbefb);

	/*
	 * setup for 2341 GHz LO:
 	 * n_lo = 1
 	 * lodiv = 2 (2^1, so set to 1)
 	 * fvco = 4782 MHz
 	 * fbkdiv = 4
 	 * ndiv = 23.91
 	 * n = 23
 	 * nummsb = 59637 (0xe8f5)
 	 * numlsb = 194 (0xc2)
 	 */
	//rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x0b9a);
	//rffc5071_reg_write(RFFC5071_P1_FREQ2, 0xe8f5);
	//rffc5071_reg_write(RFFC5071_P1_FREQ3, 0xc200);
	/* charge pump set for VCO > 3.2 GHz */
	//rffc5071_reg_write(RFFC5071_LF, 0xbefb);

	/*
	 * setup for 2411 GHz LO:
 	 * n_lo = 1
 	 * lodiv = 2 (2^1, so set to 1)
 	 * fvco = 4822 MHz
 	 * fbkdiv = 4
 	 * ndiv = 24.11
 	 * n = 24
 	 * nummsb = 7208 (0x1c28)
 	 * numlsb = 245 (0xf5)
 	 */
	//rffc5071_reg_write(RFFC5071_P1_FREQ1, 0x0c18);
	//rffc5071_reg_write(RFFC5071_P1_FREQ2, 0x1c28);
	//rffc5071_reg_write(RFFC5071_P1_FREQ3, 0xf500);
	/* charge pump set for VCO > 3.2 GHz */
	//rffc5071_reg_write(RFFC5071_LF, 0xbefb);

	/* enable device */
	rffc5071_reg_write(RFFC5071_SDI_CTRL, 0xc000); /* mixer 1 (TX) */
	//rffc5071_reg_write(RFFC5071_SDI_CTRL, 0xe000); /* mixer 2 (RX) */
}

void serial_delay(void)
{
	uint32_t i;

	for (i = 0; i < 1000; i++)
		__asm__("nop");
}

/*
 * Send 25 bits:
 *   first bit is ignored,
 *   second bit is zero for write operation,
 *   next 7 bits are register address,
 *   next 16 bits are register value.
 */
void rffc5071_reg_write(uint8_t reg, uint16_t val)
{
	int bits = 25;
	int msb = 1 << (bits -1);
	uint32_t data = ((reg & 0x7f) << 16) | val;

	/* make sure everything is starting in the correct state */
	gpio_set(PORT_MIXER, PIN_MIXER_ENX);
	gpio_clear(PORT_MIXER, (PIN_MIXER_SCLK | PIN_MIXER_SDATA));

	/*
	 * The device requires two clocks while ENX is high before a serial
	 * transaction.  This is not clearly documented.
	 */
	serial_delay();
	gpio_set(PORT_MIXER, PIN_MIXER_SCLK);

	serial_delay();
	gpio_clear(PORT_MIXER, PIN_MIXER_SCLK);

	serial_delay();
	gpio_set(PORT_MIXER, PIN_MIXER_SCLK);

	serial_delay();
	gpio_clear(PORT_MIXER, PIN_MIXER_SCLK);

	/* start transaction by bringing ENX low */
	gpio_clear(PORT_MIXER, PIN_MIXER_ENX);

	while (bits--) {
		if (data & msb)
			gpio_set(PORT_MIXER, PIN_MIXER_SDATA);
		else
			gpio_clear(PORT_MIXER, PIN_MIXER_SDATA);
		data <<= 1;

		serial_delay();
		gpio_set(PORT_MIXER, PIN_MIXER_SCLK);

		serial_delay();
		gpio_clear(PORT_MIXER, PIN_MIXER_SCLK);
	}

	serial_delay();
	gpio_set(PORT_MIXER, PIN_MIXER_ENX);
}

/*
 * Send 9 bits:
 *   first bit is ignored,
 *   second bit is one for read operation,
 *   next 7 bits are register address.
 * Then receive 16 bits (register value).
 */
uint16_t rffc5071_reg_read(uint8_t reg)
{
	int bits = 9;
	int msb = 1 << (bits -1);
	uint32_t data = 0x80 | (reg & 0x7f);

	/* make sure everything is starting in the correct state */
	gpio_set(PORT_MIXER, PIN_MIXER_ENX);
	gpio_clear(PORT_MIXER, (PIN_MIXER_SCLK | PIN_MIXER_SDATA));

	/*
	 * The device requires two clocks while ENX is high before a serial
	 * transaction.  This is not clearly documented.
	 */
	serial_delay();
	gpio_set(PORT_MIXER, PIN_MIXER_SCLK);

	serial_delay();
	gpio_clear(PORT_MIXER, PIN_MIXER_SCLK);

	serial_delay();
	gpio_set(PORT_MIXER, PIN_MIXER_SCLK);

	serial_delay();
	gpio_clear(PORT_MIXER, PIN_MIXER_SCLK);

	/* start transaction by bringing ENX low */
	gpio_clear(PORT_MIXER, PIN_MIXER_ENX);

	while (bits--) {
		if (data & msb)
			gpio_set(PORT_MIXER, PIN_MIXER_SDATA);
		else
			gpio_clear(PORT_MIXER, PIN_MIXER_SDATA);
		data <<= 1;

		serial_delay();
		gpio_set(PORT_MIXER, PIN_MIXER_SCLK);

		serial_delay();
		gpio_clear(PORT_MIXER, PIN_MIXER_SCLK);
	}

	serial_delay();
	gpio_set(PORT_MIXER, PIN_MIXER_SCLK);

	serial_delay();
	gpio_clear(PORT_MIXER, PIN_MIXER_SCLK);

	bits = 16;
	data = 0;
	/* set SDATA line as input */
	GPIO3_DIR &= ~PIN_MIXER_SDATA;

	while (bits--) {
		data <<= 1;

		serial_delay();
		gpio_set(PORT_MIXER, PIN_MIXER_SCLK);

		serial_delay();
		gpio_clear(PORT_MIXER, PIN_MIXER_SCLK);
		if (MIXER_SDATA_STATE)
			data |= 1;
	}
	/* set SDATA line as output */
	GPIO3_DIR |= PIN_MIXER_SDATA;

	serial_delay();
	gpio_set(PORT_MIXER, PIN_MIXER_ENX);

	return data;
}