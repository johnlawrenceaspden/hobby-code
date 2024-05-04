#include<stdio.h>
#include<malloc.h>
#include<assert.h>
#include<stdlib.h>
#include<stdbool.h>
#include<string.h>

void printia(int* a, int n)
{
  int i; 
  for (i=0; i<n-1; i++) printf("%d,", a[i]);
  printf("%d\n", a[i]);
}


void merge (int* nums1, int nums1Size, int m, int* nums2, int nums2Size, int n){
  int *merge=calloc(nums1Size,sizeof(int));
  int i=0,j=0;
  //printia(nums1,nums1Size);
  //printia(nums2,nums2Size);
  while(i+j<m+n && i<m && j<n){
    //printia(merge, nums1Size);
    //printf("%d,%d:%d %d ->", i,j,nums1[i],nums2[j] );

    if(nums1[i]<nums2[j]){
      merge[i+j]=nums1[i];
      i++;
    }else{
      merge[i+j]=nums2[j];
      j++;
    }
    //printia(merge, nums1Size);
  }
  while(i<m){
      merge[i+j]=nums1[i];
      i++;
  }
  while(j<n){
      merge[i+j]=nums2[j];
      j++;
  }

  //printia(merge, nums1Size);
  for(int i=0; i<nums1Size; i++) nums1[i]=merge[i];
}


void Test(int* nums1, int nums1Size, int m, int* nums2, int nums2Size, int n)
{
  printia(nums1,nums1Size);
  printia(nums2,nums2Size);

  merge(nums1, nums1Size, m, nums2, nums2Size, n);
  printia(nums1,nums1Size);
}


int main(void)
{
  printf("yo\n");
  int nums1[]={1,2,3,0,0,0};
  int m=3;
  int nums2[]={2,5,6};
  int n=3;
  int nums1Size=sizeof(nums1)/sizeof(int);
  int nums2Size=sizeof(nums2)/sizeof(int);
  

  Test(nums1, nums1Size, m, nums2, nums2Size, n);

  return 0;
}
