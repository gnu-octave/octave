# /*
cc -s -o octtopnm octtopnm.c
exit
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* usage: octtopnm [-a] octfile */

static void usage(message)
char *message;
{
  if(message != NULL) {
    fprintf(stderr,"octtopnm: %s\n",message);
  }
  fprintf(stderr,"usage: octtopnm [-a] octavefile\n");
  exit(1);
}

static void fatal(message)
char *message;
{
  if(message != NULL) {
    fprintf(stderr,"octtopnm: %s\n",message);
  }
  exit(1);
}

int main(argc, argv)
int argc;
char **argv;
{
  int rawbits = 1, row, col, index;
  int cmap_rows, cmap_cols, img_rows, img_cols;
  int gray, pbm, pgm, ppm;
  unsigned char **rgb, byte;
  unsigned short **img;
  char *oct_file_name;
  FILE *oct_file;
  char cmap_name[4], cmap_type[7], img_name[2], img_type[7];
  double mat_val;
  int option;
  extern char *optarg;
  extern int optind;

  if(argc == 1) {
    usage(NULL);
  }

  while((option = getopt(argc,argv,"ha")) != EOF) {
    switch(option) {
    case 'h':
      /* help */
      usage(NULL);
      break;
    case 'a':
      rawbits = 0;
      break;
    case '?':
    default:
      usage("unrecognized option");
    }
  }

  if(optind+1 != argc) {
    usage("input file name missing");
  }

  oct_file_name = argv[optind];
  if((oct_file = fopen(oct_file_name,"r")) == NULL) {
    fatal("unable to open input file");
  }

  if(fscanf(oct_file,"# name: %s\n",cmap_name) != 1 || 
     strcmp(cmap_name,"map") != 0) {
    fatal("not a valid octave image file");
  }

  if(fscanf(oct_file,"# type: %s\n",cmap_type) != 1 || 
     strcmp(cmap_type,"matrix") != 0) {
    fatal("not a valid octave image file");
  }

  if(fscanf(oct_file,"# rows: %d\n",&cmap_rows) != 1) {
    fatal("error reading octave image file");
  }

  if(fscanf(oct_file,"# columns: %d\n",&cmap_cols) != 1) {
    fatal("error reading octave image file");
  }

  if(cmap_cols != 3) {
    fatal("invalid color map in octave image file");
  }

  if((rgb = (unsigned char **)
      malloc(cmap_rows*sizeof(unsigned char *))) == NULL) {
    fatal("out of memory");
  }

  if((rgb[0] = (unsigned char *)
      malloc(cmap_rows*cmap_cols*sizeof(unsigned char))) == NULL) {
    fatal("out of memory");
  }

  for(row=1; row<cmap_rows; row++) {
    rgb[row] = rgb[row-1]+3;
  }

  gray = 1;
  for(row=0; row<cmap_rows; row++) {
    for(col=0; col<cmap_cols; col++) {
      if(fscanf(oct_file,"%lf",&mat_val) != 1) {
        fatal("error reading color map entries");
      }
      if(mat_val < 0) mat_val = 0.;
      if(mat_val > 1) mat_val = 1.;
      rgb[row][col] = mat_val*255;
    }
    if(gray) {
      if(rgb[row][0] != rgb[row][1] || rgb[row][0] != rgb[row][2]) {
        /* It's a color image. */
        gray = 0;
      }
    }
  }

  if(fscanf(oct_file,"\n# name: %s\n",img_name) != 1 || 
     strcmp(img_name,"X") != 0) {
    fatal("not a valid octave image file");
  }

  if(fscanf(oct_file,"# type: %s\n",img_type) != 1 || 
     strcmp(img_type,"matrix") != 0) {
    fatal("not a valid octave image file");
  }

  if(fscanf(oct_file,"# rows: %d\n",&img_rows) != 1) {
    fatal("error reading octave image file");
  }

  if(fscanf(oct_file,"# columns: %d\n",&img_cols) != 1) {
    fatal("error reading octave image file");
  }

  if((img = (unsigned short **)
      malloc(img_rows*sizeof(unsigned short *))) == NULL) {
    fatal("out of memory");
  }

  if((img[0] = (unsigned short *)
      malloc(img_rows*img_cols*sizeof(unsigned short))) == NULL) {
    fatal("out of memory");
  }

  for(row=1; row<img_rows; row++) {
    img[row] = img[row-1]+img_cols;
  }

  for(row=0; row<img_rows; row++) {
    for(col=0; col<img_cols; col++) {
      if(fscanf(oct_file,"%lf",&mat_val) != 1) {
        fatal("error reading color map entries");
      }
      if(mat_val < 1) mat_val = 1.;
      if(mat_val > cmap_rows) mat_val = cmap_rows;
      img[row][col] = mat_val;
    }
  }

  pbm = pgm = ppm = 0;

  if(cmap_rows == 2 && gray && 
     ((rgb[0][0] == 0 && rgb[1][0] == 255) ||
      (rgb[0][0] == 255 && rgb[1][0] == 0))) {
    /* Create a bitmap only if there are two colormap entries and they are
       black and white. */
    pbm = 1;
  }
  else if(gray) {
    /* If not a bitmap, create a gray scale image if the entries within
       each row of the color map are equal. */
    pgm = 1;
  }
  else {
    /* Otherwise create a full color image. */
    ppm = 1;
  }

  if(rawbits) {
    if(pbm) {
      printf("P4\n");
      printf("%d %d\n",img_cols,img_rows);
      index = 0;
      for(row=0; row<img_rows; row++) {
        for(col=0; col<img_cols; col++) {
          if(index == 7) {
            byte = (byte << 1) + !rgb[img[row][col]-1][0];
            fwrite(&byte,sizeof(unsigned char),1,stdout);
            byte = 0;
            index = 0;
          }
          else {
            byte = (byte << 1) + !rgb[img[row][col]-1][0];
            index++;
          }
        }
      }
      if(index != 0) {
        printf("\n");
      }
    }
    else if(pgm) {
      printf("P5\n");
      printf("%d %d\n",img_cols,img_rows);
      printf("255\n");
      for(row=0; row<img_rows; row++) {
        for(col=0; col<img_cols; col++) {
          fwrite(rgb[img[row][col]-1],sizeof(unsigned char),1,stdout);
        }
      }
    }
    else {
      printf("P6\n");
      printf("%d %d\n",img_cols,img_rows);
      printf("255\n");
      for(row=0; row<img_rows; row++) {
        for(col=0; col<img_cols; col++) {
          fwrite(rgb[img[row][col]-1],sizeof(unsigned char),3,stdout);
        }
      }
    }
  }
  else {
    if(pbm) {
      printf("P1\n");
      printf("%d %d\n",img_cols,img_rows);
      index = 0;
      for(row=0; row<img_rows; row++) {
        for(col=0; col<img_cols; col++) {
          if(index == 30) {
            printf("%d\n",!rgb[img[row][col]-1][0]);
            index = 0;
          }
          else {
            printf("%d ",!rgb[img[row][col]-1][0]);
            index++;
          }
        }
      }
      if(index != 0) {
        printf("\n");
      }
    }
    else if(pgm) {
      printf("P2\n");
      printf("%d %d\n",img_cols,img_rows);
      printf("255\n");
      index = 0;
      for(row=0; row<img_rows; row++) {
        for(col=0; col<img_cols; col++) {
          if(index == 12) {
            printf("%d\n",rgb[img[row][col]-1][0]);
            index = 0;
          }
          else {
            printf("%d ",rgb[img[row][col]-1][0]);
            index++;
          }
        }
      }
      if(index != 0) {
        printf("\n");
      }
    }
    else {
      printf("P3\n");
      printf("%d %d\n",img_cols,img_rows);
      printf("255\n");
      index = 0;
      for(row=0; row<img_rows; row++) {
        for(col=0; col<img_cols; col++) {
          if(index == 4) {
            printf("%d %d %d\n",rgb[img[row][col]-1][0],
                   rgb[img[row][col]-1][1],rgb[img[row][col]-1][2]);
            index = 0;
          }
          else {
            printf("%d %d %d ",rgb[img[row][col]-1][0],
                   rgb[img[row][col]-1][1],rgb[img[row][col]-1][2]);
            index++;
          }
        }
      }
      if(index != 0) {
        printf("\n");
      }
    }
  }

  return 0;
}
