struct test
{
	int a, b, c, d, e, f, g, h, i;
};

int global = 9;
int global2 = 10;

int MyTest( int a, unsigned char ll, unsigned char b, struct test t ) 
{
	int r;
	int k;
	a+=353*4;
	global +=5;
	ll = ~ll;
	r = a+ll+b;
	r = r - 5;
	r = 5 - r;
	k = t.g * ll;
	if( k == 36 )
	{
		int m = r * k;
		r += m;
	}
	else
	{
		r+= 9;
	}
	r = t.a;
	r += 9+a;
	r += global;

	return r * k;
}

int quickret( int a )
{
	return a+5;
}

int strtest( const char * st )
{
	int ret = 0;
	int c;
	while( (c = *(st++)) )
		ret += c;
	return ret;
}

void AnotherTest( int k )
{
	int a;
	int r;
	for( a = 0; a < k; a++ )
	{
		r = r + 1;
	}
	//global = global + 5;
}

void VoidTest()
{
reglob:
	global++;
	if( global < 100 ) goto reglob;
}

int _start()
{
	struct test t;
	t.a = 4;
	t.g = 6;
	global++;
	global2++;
	VoidTest();
	AnotherTest(1);
	strtest( "hello, world" );
	return MyTest( 0xdeadbeef, 9, 6, t );
}

