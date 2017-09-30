var cpusp = 1048576;
var cpua0 = 0|0;
var cpua1 = 0|0;
var cpua2 = 0.0;
var cpua3 = 0.0;
var memarray = new Uint8Array(cpusp);

function tcc_write( location, size, value ) {
	if( typeof value === 'number' )
	{
		for( var i = 0; i < ((size&~3)+4)|0; i+=4 )
		{
			memarray[location + i] = value;
			value>>=32;
		}
	} else {
		for( var i = 0; i < ((size&~3)+4)|0; i++ )
		{
			var v = 0;
			for( var j = 0; j < 4; j++ )
			{
				v |= value[j+i*4]<<(j*8);
			}
			memarray[location + i] = v;
		}
	}
}
function tcc_writereg( location, value )
{
	tcc_write( location, 4, value );
}
function tcc_read( location, size, type )
{
	if( type === 'number' )
	{
		var ret = 0;
		for( var i = 0; i < (((size&~3)+4)|0); i+=4 )
		{
			ret <<= 32;
			ret |= memarray[location + i];
		}
		return ret;
	} else {
		var ret = new Uint8Array( size );
		for( var i = 0; i < ((size&~3)+4)|0; i++ )
		{
			var v = 0;
			for( var j = 0; j < 4; j++ )
			{
				v |= memarray[location + j+i*4]<<(j*8);
			}
			ret[i] = v;
		}
		return ret;
	}
}
function tcc_readreg( location )
{
	return tcc_read( location, 4, 'number' );
}
function tcc_pop( size, type )
{
	var ret = tcc_read( cpusp, size, type );
	cpusp += size;
	return ret;
}
function tcc_push( size, val )
{
	cpusp -= 4;
	var ret = tcc_write( cpusp, 4, val );
	return ret;
}
function float32touint32( f )
{
	var farr = new Float32Array(1);
	farr[0] = f;
	var iarr = new Uint32Array(farr.buffer);
	return iarr[0];
}
function uint32tofloat32( f )
{
	var iarr = new Uint32Array(1);
	iarr[0] = f;
	var farr = new Float32Array(iarr.buffer);
	return farr[0];
}
function float64touint64( f )
{
	var farr = new Float64Array(1);
	farr[0] = f;
	var iarr = new Uint64Array(farr.buffer);
	return iarr[0];
}
function uint64tofloat64( f )
{
	var iarr = new Uint64Array(1);
	iarr[0] = f;
	var farr = new Float64Array(iarr.buffer);
	return farr[0];
}
function utos32( f )
{
	return (new Int32Array([f]))[0];
}
