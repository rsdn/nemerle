/* The GetOffsets function is a slightly modified version of the
 * TOC reading function from discid.c by Jeremy D. Zawodny */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/cdrom.h>

#define MAX 1000

void AppIntToString(int num, char *arr, int *idx);
char* GetOffsets(char *dev);

char* 
__attribute__ ((visibility ("default")))
GetOffsets(char *dev)
{
 static char ret[MAX];
 struct toc { int min, sec, frame; } cdtoc[100];
 int i,cd,err[3],idx=0;
 struct cdrom_tochdr tochdr;
 struct cdrom_tocentry tocentry;

 if ((cd = open(dev, O_RDONLY | O_NONBLOCK))==-1) {
//  err[0] = errno;	 
  return "";
 }
 if (ioctl(cd, CDROMREADTOCHDR, &tochdr)==-1) {
//  err[1] = errno;
  return "";
 }
  
 for (i = tochdr.cdth_trk0; i <= tochdr.cdth_trk1; i++) {
  tocentry.cdte_track = i; 
  tocentry.cdte_format = CDROM_MSF;
  ioctl(cd, CDROMREADTOCENTRY, &tocentry);
  cdtoc[i-1].min = tocentry.cdte_addr.msf.minute;
  cdtoc[i-1].sec = tocentry.cdte_addr.msf.second;
  cdtoc[i-1].frame = tocentry.cdte_addr.msf.frame;
  cdtoc[i-1].frame += cdtoc[i-1].min*60*75;
  cdtoc[i-1].frame += cdtoc[i-1].sec*75;
 }
 tocentry.cdte_track = 0xAA;
 tocentry.cdte_format = CDROM_MSF;
 ioctl(cd, CDROMREADTOCENTRY, &tocentry);
 cdtoc[tochdr.cdth_trk1].min = tocentry.cdte_addr.msf.minute;
 cdtoc[tochdr.cdth_trk1].sec = tocentry.cdte_addr.msf.second;
 cdtoc[tochdr.cdth_trk1].frame = tocentry.cdte_addr.msf.frame;
 cdtoc[tochdr.cdth_trk1].frame += cdtoc[tochdr.cdth_trk1].min*60*75;
 cdtoc[tochdr.cdth_trk1].frame += cdtoc[tochdr.cdth_trk1].sec*75;
 close(cd);

 AppIntToString(tochdr.cdth_trk1,ret,&idx);
 for (i = 0; i < tochdr.cdth_trk1; i++) 
  AppIntToString(cdtoc[i].frame,ret,&idx);
 AppIntToString((cdtoc[tochdr.cdth_trk1].frame)/75,ret,&idx);
 ret[--idx]='\0';
 
 return ret;
}

void
__attribute__ ((visibility ("hidden")))
AppIntToString(int num, char *arr, int *idx) 
{
 int i;
 char tmp[MAX];
 for(i=0;num>0;++i) {
  tmp[i]=num-((num/10)*10)+'0';
  num/=10;
 }
 arr[*idx+i]=' ';
 arr[*idx+i+1]='\0';
 --i;
 while (i>=0)  
  arr[(*idx)++]=tmp[i--];

 ++(*idx); 
}
