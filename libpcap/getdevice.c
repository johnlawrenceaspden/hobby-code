/**
http://www.devdungeon.com/content/using-libpcap-c
sudo yum install libpcap-devel
sudo apt-get install libpcap-dev
gcc -lpcap hello.c
**/

#include <stdio.h>
#include <pcap.h>

int main(void)
{

  char *device; /* Name of device (e.g. eth0, wlan0) */
  char error_buffer[PCAP_ERRBUF_SIZE]; /* Size defined in pcap.h */

  /* Find a device */
  device = pcap_lookupdev(error_buffer);
  if (device == NULL) {
    printf("Error finding device: %s\n", error_buffer);
    return 1;
  }

  printf("Network device found: %s\n", device);
  return 0;
}
