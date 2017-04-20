/**

http://www.devdungeon.com/content/using-libpcap-c
http://stackoverflow.com/questions/7309773/c-writing-structs-to-a-file-pcap
http://seclists.org/tcpdump/2010/q2/22

sudo yum install libpcap-devel
sudo apt-get install libpcap-dev
gcc -lpcap hello.c

examine emitted packets with:
wireshark /tmp/capture.pcap

**/

#include <stdio.h>
#include <pcap.h>

/* Ethernet/IP/SCTP INIT chunk */
static const unsigned char pkt1[82] = {
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* ........ */
  0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x45, 0x00, /* ......E. */
  0x00, 0x44, 0x55, 0xb1, 0x00, 0x00, 0x40, 0x84, /* .DU...@. */
  0x26, 0x83, 0x7f, 0x00, 0x00, 0x01, 0x7f, 0x00, /* &....... */
  0x00, 0x01, 0x00, 0x01, 0x1f, 0x90, 0x00, 0x00, /* ........ */
  0x00, 0x00, 0x68, 0xe5, 0x88, 0x1b, 0x01, 0x00, /* ..h..... */
  0x00, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0f, /* .$...... */
  0xa0, 0x00, 0x00, 0x04, 0xff, 0xff, 0x00, 0x00, /* ........ */
  0x16, 0x2e, 0x80, 0x00, 0x00, 0x04, 0xc0, 0x00, /* ........ */
  0x00, 0x04, 0x00, 0x0c, 0x00, 0x06, 0x00, 0x05, /* ........ */
  0x00, 0x00                                      /* .. */
};


int main(void)
{
    pcap_t *pd;
    pcap_dumper_t *pdumper;

    pd = pcap_open_dead(DLT_RAW, 65535 /* snaplen */);

    /* Create the output file. */
    pdumper = pcap_dump_open(pd, "/tmp/capture.pcap");
    
    struct pcap_pkthdr pcap_hdr;
    pcap_hdr.caplen = sizeof(pkt1);
    pcap_hdr.len = pcap_hdr.caplen;

    for (int i=0; i<10; i++) {

        /* write packet to savefile */
      pcap_dump((u_char *)pdumper, &pcap_hdr, pkt1);
    }

    pcap_close(pd);
    pcap_dump_close(pdumper);

    return 0;
}
