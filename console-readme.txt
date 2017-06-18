Mosaic image generator, version 0.2.
This program generates a mosaic from given images and base image.

Command Line:
	mosaic <input files> <options>

Like:
	mosaic cat.jpg angry.jpg
	mosaic cat.jpg -threads 2

Available command line options:
	-output		<file name> 		- changes output file name. Makes sense only when rendering one image.
	-size		<width>x<height>	- changes mosaic images size (the images that the result is built with). Default: 10x10
	-wait					- waits for the 'enter' key after rendering is finished.
	-threads	<thread count>		- changes workers (thread) count. Default: 4.
	-cache		true/1/false/0		- enables/disables image cache. Default: true.
	-reuse		true/1/false/0		- enables/disables subimage reusing mode. Default: true.
	-purge-cache				- removes the 'cache' directory.

Subimage reusing mode:
	When enabled, two or more the same subimages will be placed next to each other (if they fit the image); if disabled, the generator will try to find some other subimage (so that there's a small difference). It's disabled by the default, but you can enable it if you think that the output image is 'ugly' - it may look better (or worse, you have to find out). The best is visible on the "cat_hq.jpg" sample. It can slightly impact the render time.