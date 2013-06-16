--
-- Copyright 2012 Jared Boone
-- Copyright 2013 Benjamin Vernoux
--
-- This file is part of HackRF.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street,
-- Boston, MA 02110-1301, USA.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

library UNISIM;
use UNISIM.vcomponents.all;

entity top is
    Port(
        HOST_DATA       : inout std_logic_vector(7 downto 0);
        HOST_CAPTURE    : out   std_logic;
        HOST_DISABLE    : in    std_logic;
        HOST_DIRECTION  : in    std_logic;

        DA              : in    std_logic_vector(7 downto 0);
        DD              : out   std_logic_vector(9 downto 0);

        CODEC_CLK       : in    std_logic;
        CODEC_X2_CLK    : in    std_logic;

        B1AUX           : inout std_logic_vector(16 downto 9);
        B2AUX           : inout std_logic_vector(16 downto 1)
    );

end top;

architecture Behavioral of top is
    signal codec_clk_i : std_logic;
    signal adc_data_i : std_logic_vector(7 downto 0);
    signal dac_data_o : std_logic_vector(9 downto 0);

    signal host_clk_i : std_logic;

    type transfer_direction is (from_adc, to_dac);
    signal transfer_direction_i : transfer_direction;

    signal host_data_enable_i : std_logic;
    signal host_data_capture_o : std_logic;

    signal data_from_host_i : std_logic_vector(7 downto 0);
    signal data_to_host_o : std_logic_vector(7 downto 0);

    constant DCOFS_BITS : integer := 5;
    signal dcofs_clk    : std_logic;
    signal dcofs_bit    : std_logic;
    signal dcofs_apply  : std_logic;
    signal dcofs_sreg   : std_logic_vector(2*DCOFS_BITS-1 downto 0) := (others => '0');
    signal dcofs_freg   : std_logic_vector(2*DCOFS_BITS-1 downto 0) := (others => '0');
    signal dcofs_mux    : std_logic_vector(7 downto 0);

begin

    B1AUX(16 downto 12) <= (others => '0');
    B1AUX(11 downto  9) <= (others => 'Z');
    -- B2AUX <= (others => '0');
    B2AUX <= (16 downto DCOFS_BITS*2+1 => '0') & dcofs_freg;

    ------------------------------------------------
    -- Codec interface

    adc_data_i <= DA(7 downto 0);
    DD(9 downto 0) <= dac_data_o;

    ------------------------------------------------
    -- Clocks

    codec_clk_i <= CODEC_CLK;

    BUFG_host : BUFG
    port map (
        O => host_clk_i,
        I => CODEC_X2_CLK
    );

    ------------------------------------------------
    -- SGPIO interface

    HOST_DATA <= data_to_host_o when transfer_direction_i = from_adc
                                else (others => 'Z');
    data_from_host_i <= HOST_DATA;

    HOST_CAPTURE <= host_data_capture_o;
    host_data_enable_i <= not HOST_DISABLE;
    transfer_direction_i <= to_dac when HOST_DIRECTION = '1'
                                   else from_adc;

    ------------------------------------------------

    process(host_clk_i)
    begin
        if rising_edge(host_clk_i) then
            data_to_host_o <= adc_data_i + dcofs_mux;
        end if;
    end process;

    process(host_clk_i)
    begin
        if rising_edge(host_clk_i) then
            if transfer_direction_i = to_dac then
                dac_data_o <= data_from_host_i & "00";
            else
                dac_data_o <= (dac_data_o'high => '1', others => '0');
            end if;
        end if;
    end process;

    process(host_clk_i, codec_clk_i)
    begin
        if rising_edge(host_clk_i) then
            if transfer_direction_i = to_dac then
                if codec_clk_i = '1' then
                    host_data_capture_o <= host_data_enable_i;
                end if;
            else
                if codec_clk_i = '0' then
                    host_data_capture_o <= host_data_enable_i;
                end if;
            end if;
        end if;
    end process;


    ------------------------------------------------
    -- DC offset register

    dcofs_clk   <= B1AUX(9);
    dcofs_bit   <= B1AUX(10);
    dcofs_apply <= B1AUX(11);

    process (dcofs_clk)
    begin
        if rising_edge(dcofs_clk) then
            dcofs_sreg <= dcofs_sreg(dcofs_sreg'left-1 downto 0) & dcofs_bit;
        end if;
    end process;

    process (host_clk_i)
    begin
        if rising_edge(host_clk_i) then
            if dcofs_apply = '1' then
                dcofs_freg <= dcofs_sreg;
            end if;
        end if;
    end process;

    dcofs_mux(DCOFS_BITS-1 downto 0) <=
        dcofs_freg(2*DCOFS_BITS-1 downto DCOFS_BITS)
            when codec_clk_i = '1' else dcofs_freg(DCOFS_BITS-1 downto 0);

    dcofs_mux(7 downto DCOFS_BITS) <= (others => dcofs_mux(DCOFS_BITS-1));

end Behavioral;
