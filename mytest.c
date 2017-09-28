struct test
{
	int a, b, c, d, e, f, g, h, i;
};

int global = 9;

int MyTest( int a, unsigned char ll, unsigned char b, struct test t ) 
{
	int r;
	int k;
	a+=353*4;
	ll = ~ll;
	r = a+ll+b;
	k = t.g * ll;
	if( k == 36 )
	{
		int m = r * k;
		r += m;
	}
	r = t.a;
	r += 9+a;
	r += global;

	return r * k;
}

void AnotherTest( int k )
{
	int a;
	for( a = 0; a < k; a++ )
	{
		r = r + 1;
	}
	return r;
}

int _start()
{
	struct test t;
	t.a = 4;
	t.g = 6;
	return MyTest( 0xdeadbeef, 9, 6, t );
}

