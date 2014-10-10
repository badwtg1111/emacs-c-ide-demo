/*
 * Htags Hyper-text Reference Kit.
 *
 * This file is placed into the public domain by the author,
 * 2005, 2006 Tama Communications Corporation.
 *
 * Two library function, qsort(3) and bsearch(3), are required.
 */
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/stat.h>

#if defined(_WIN32) || defined(__DJGPP__)
# define OPENMODE O_BINARY
#else
# define OPENMODE 0
#endif

int htags_load_filemap(const char *);
void htags_unload_filemap(void);
int htags_path2url(const char *, int, char *, int);

/** @file
 * @NAME{htags} Hyper-text Reference Kit.
 *
 * Usage by example.
 *
 * Getting the URL of @FILE{i386/i386/identcpu.c} at 120, in variable url.
 *
 * @code
 *	char url[MAXPATHLEN];
 *	-- Load filemap generated by htags(1).
 *	ret = htags_load_filemap("HTML/FILEMAP");
 *	if (ret != 0)
 *		ERROR;
 *	-- Get URL of the specified file name and line number.
 *	ret = htags_path2url("i386/i386/identcpu.c", 120, url, sizeof(url));
 *	if (ret != 0)
 *		ERROR;
 *	url == 'S/1238.html#L120'
 *	-- release resource
 *	htags_unload_filemap();
 * @endcode
 *
 * Since the URL is a relative path from the HTML directory, you should
 * modify it so that your application program works well.
 */
/**
 * in memory FILEMAP
 *
 * Alphabetical sorted table.
 */
struct map
{
	const char *name;
	const char *path;
};
/**
 * @name Global variables.
 *
 * These variables are set by htags_load_filemap(), and referred by <br>
 * htags_path2url() and htags_unload_filemap().
 */
/** @{ */
static char *global_contents;		/**< filemap contents		*/
static struct map *global_map;		/**< file -\> url mapping table	*/
static int global_maplines;		/**< the lines of #global_map	*/
/** @} */

/*----------------------------------------------------------------------*/
/* Local functions							*/
/*----------------------------------------------------------------------*/
/**
 * load contents of FILEMAP into the memory.
 *
 *	@param[in]	file	path of FILEMAP
 *	@param[out]	area	file contents (malloced)
 *	@param[out]	size	size of @a area
 *	@return	0 succesful <br>
 *		-1: out of memory. <br>
 *		-2: FILEMAP not found. <br>
 *		-3: cannot fstat FILEMAP. <br>
 *		-4: cannot read FILEMAP.
 */
static int
load_filemap_contents(const char *file, char **area, int *size)
{
	struct stat st;
	char *p = NULL;
	int status = 0;
	int fd = -1;

	/* Load FILEMAP contents */
	status = -2;
	if ((fd = open(file, OPENMODE)) < 0)
		goto err;
	status = -3;
	if (fstat(fd, &st) < 0)
		goto err;
	status = -1;
	if ((p = (char *)malloc(st.st_size)) == NULL)
		goto err;
	status = -4;
	if (read(fd, p, st.st_size) != st.st_size)
		goto err;
	close(fd);
	*area = p;
	*size = st.st_size;
	return 0;
err:
	if (fd != -1)
		close(fd);
	if (p != NULL)
		free(p);
	return status;
}

/**
 * comparison function for @CODE{bsearch()}.
 */
static int
cmp(const void *s1, const void *s2)
{
	return strcmp(((struct map *)s1)->name, ((struct map *)s2)->name);
}
/**
 * creates index for in core FILEMAP.
 *
 *	@param[in]	area	filemap contents
 *	@param[in]	size	size of @a area
 *	@param[out]	map	map structure
 *	@param[out]	lines	@a map lines
 *	@return	 0: succesful <br>
 *		-1: out of memory. <br>
 *		-5: invalid format.
 */
static int
create_filemap_index(char *area, int size, struct map **map, int *lines)
{
	char *p, *endp = area + size;
	struct map *m;
	int n = 0;
	int status;
	int i;

	/* Count line number */
	for (p = area; p < endp; p++)
		if (*p == '\n')
			n++; 
	status = -1;
	if ((m = (struct map *)malloc(sizeof(struct map) * n)) == NULL)
		return -1;
	/*
	 * FILEMAP format:
	 * <NAME>\t<PATH>\n
	 */
	p = area;
	for (i = 0; i < n; i++) {
		m[i].name = p;
		while (*p && *p != '\t')
			p++;
		if (*p == '\0')
			goto ferr;
		*p++ = '\0';
		m[i].path = p;
		while (*p && *p != '\r' && *p != '\n')
			p++;
		if (*p == '\0')
			goto ferr;
		if (*p == '\r')
			*p++ = '\0';
		if (*p == '\n')
			*p++ = '\0';
	}
	qsort(m, n, sizeof(struct map), cmp);
	*map = m;
	*lines = n;
	return 0;
ferr:
	free(m);
	return -5;
}

/**
 * unloads FILEMAP.
 */
static void
unload_filemap(void)
{
	(void)free(global_map);
	global_map = NULL;
	(void)free(global_contents);
	global_contents = NULL;
}
/*----------------------------------------------------------------------*/
/* Global functions							*/
/*----------------------------------------------------------------------*/
/**
 * loads FILEMAP.
 *
 *	@param[in]	filemap	FILEMAP file
 *	@par Globals used (output):
 *		#global_contents	filemap contents (for free) <br>
 *		#global_map	filemap index. <br>
 *		#global_maplines	lines of filemap index.
 *
 *	@return		 0: succesful <br>
 *			-1: out of memory. <br>
 *			-2: FILEMAP not found. <br>
 *			-3: cannot fstat FILEMAP. <br>
 *			-4: cannot read FILEMAP. <br>
 *			-5: format error.
 */
int
htags_load_filemap(const char *filemap)
{
	int status = 0;
	char *area;
	int size, lines;
	struct map *map;

	status = load_filemap_contents(filemap, &area, &size);
	if (status < 0)
		return status;
	status = create_filemap_index(area, size, &map, &lines);
	if (status < 0) {
		(void)free(area);
		return status;
	}
	global_contents = area;
	global_map = map;
	global_maplines = lines;
	return 0;
}
/**
 * unloads FILEMAP.
 */
void
htags_unload_filemap(void)
{
	unload_filemap();
}

/**
 * convert file path name into the URL in the hypertext.
 *
 *	@param[in]	path	path name in the filesystem.
 *	@param[in]	line	0: ignored, !=0: line number in @a path.
 *	@param[out]	url	result url
 *	@param[in]	size	size of @a url
 *	@par Globals used (input):
 *		#global_contents	filemap contents (for free) <br>
 *		#global_map	filemap index. <br>
 *		#global_maplines	lines of filemap index.
 *
 *	@return		0: succesful <br>
 *			1: path not found <br>
 *			-1: filemap not loaded yet
 *
 * URL: @CODE{S/\<file id\>.html\#\<line number\>}
 */
int
htags_path2url(const char *path, int line, char *url, int size)
{
	struct map tmp;
	struct map *result;

	if (global_contents == NULL || global_map == NULL)
		return -1;
	tmp.name = path;
	result = (struct map *)bsearch(&tmp, global_map, global_maplines, sizeof(struct map), cmp);
	if (result == NULL)
		return 1;
	if (line > 0)
		snprintf(url, size, "%s#L%d", result->path, line);
	else
		snprintf(url, size, "%s", result->path);
	return 0;
}

#ifdef TEST

/**
 * @par Usage:
 * @NAME{htags_path2url} @ARG{filemap} @ARG{path} [@ARG{line}]
 *
 * @par Examples:
 * @code
 * $ htags_path2url HTML/FILEMAP i386/i386/identcpu.c 120
 * i386/i386/identcpu.c, line 120 => S/1238.html#L120
 * $ _
 * @endcode
 */
int
main(int argc, char **argv)
{
	char url[MAXPATHLEN];
	char *path, *html;
	int line = 0;
	int ret;

	if (argc < 3) {
		fprintf(stderr, "Usage: htags_path2url filemap path [line]\n");
		exit(1);
	}
	html = argv[1];
	path = argv[2];
	if (argc > 3)
		line = atoi(argv[3]);
	ret = htags_load_filemap(html);
	if (ret != 0) {
		fprintf(stderr, "htags_load_filemap failed.(");
		switch (ret) {
		case -1: fprintf(stderr, "out of memory"); break;
		case -2: fprintf(stderr, "FILEMAP not found."); break;
		case -3: fprintf(stderr, "cannot fstat FILEMAP."); break;
		case -4: fprintf(stderr, "cannot read FILEMAP."); break;
		case -5: fprintf(stderr, "format error."); break;
		default: fprintf(stderr, "unknown error."); break;
		}
		fprintf(stderr, ")\n");
		exit(1);
	}
	ret = htags_path2url(path, line, url, sizeof(url));
	if (ret != 0) {
		fprintf(stderr, "htags_path2url failed.(");
		switch (ret) {
		case  1: fprintf(stderr, "path not found."); break;
		case -1: fprintf(stderr, "out of memory."); break;
		}
		fprintf(stderr, ")\n");
		exit(1);
	}
	fprintf(stdout, "%s, line %d => %s\n", path, line, url);
	htags_unload_filemap();
	return 0;
}
#endif