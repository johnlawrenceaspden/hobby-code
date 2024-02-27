#include<stdio.h>
#include<malloc.h>
#include<assert.h>

/**
 * Note: The returned array must be malloced, assume caller calls free().
 */
int* twoSum(int* nums, int numsSize, int target, int* returnSize) {
  int* i = nums;
  int* j = nums + numsSize-1;
  //printf("%d + %d = %d\n", *i, *j, *i + *j);
  while (*i+*j != target && j>=i) {
    if (*i + *j > target){ j--;}
    else {i++;}
    //printf("%d + %d = %d\n", *i, *j, *i + *j);

  }
  int* retval=(int*) malloc(2*sizeof(int));
  *returnSize=2;
  retval[0]=i-nums;
  retval[1]=j-nums;
  return retval;
}


void aiprint(int *array, int arraysize)
{
  printf("{");
  for(int i=0; i<arraysize-1; i++){
    printf("%d,", array[i]);
  }
  printf("%d}", array[arraysize-1]);
}


void twoSumTest(int*nums, int numsize, int target)
{
  int* retval;
  int returnSize;

  aiprint(nums, numsize);
  printf(" =%d\n", target);
  retval = twoSum(nums, numsize, target, &returnSize);
  aiprint(retval, returnSize); printf("\n");

  int a=*(nums+retval[0]);
  int b=*(nums+retval[1]);
  int c = a+b;
  printf("%d + %d = %d\n",a,b,c);
  free(retval);
  if(target==a+b) printf("yay\n"); else printf("BOO!\n");
}



int main(void)
{
  printf("yo\n");

  {
    int nums[]={2,7,11,15};
    int target=12;
    twoSumTest(nums, sizeof(nums)/sizeof(int), target);
  }

  {
    int nums[]={2,7,11,15};
    int target=14;
    twoSumTest(nums, sizeof(nums)/sizeof(int), target);
  }

  
  return 0;
}
