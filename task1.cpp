#include <iostream>
#include <fstream> // std::ifstream
#include <cstdio>
#include <iomanip> // std::setprecision & std::fixed
#include <cmath> // round, pow
using namespace std;

#define DIM_ERR "Error: the dimensional problem occurred"
#define GNUPLOT_CMD "C:\\gnuplot\\bin\\gnuplot -persist"
#define HORISONTAL 0
#define VERTICAL 1
#define FLOAT_ERR 0.0000001

template<typename V>
class Matrix
{
public:
    // Constructor; creates matrix with dimensions width X height,
    // if size is not given creates empty matrix.
    Matrix(int height = 0, int width = 0)
    {
        content = (V**) malloc(height * sizeof(V*));
        for(int r = 0; r < height; ++r)
            content[r] = (V*) malloc(width * sizeof(V));
        this->width = width;
        this->height = height;
    }

    // Copy constructor; creates new matrix equal to other.
    Matrix(Matrix& other) : Matrix(0, 0) { (*this) = other; }

    // Destructor; frees memory where matrix is stored.
    ~Matrix()
    {
        for(int i = 0; i < height; ++i) free(content[i]);
        free(content);
    }

    // Resizes this matrix. No operations are performed on content
    // (elements outside resized matrix will be lost and newly added elements
    // can store trash; however, all other elements will retain their values).
    // Returns reference to this matrix.
    Matrix& resize(int height, int width)
    {
        this->content = (V**) realloc(this->content, height * sizeof(V*));
        int lenRealloc = this->height < height ? this->height : height;
        for(int r = 0; r < lenRealloc; ++r)
            this->content[r] = (V*) realloc(this->content[r], width * sizeof(V));
        for(int r = lenRealloc; r < height; ++r)
            this->content[r] = (V*) malloc(width * sizeof(V));
        this->width = width;
        this->height = height;

        return *this;
    }

    /// @brief Creates and returns matrix, which stores values Mij of this
    /// matrix, where r1 <= i < r2, c1 <= j < c2, all values 0-indexed
    Matrix& submatrix(int r1, int c1, int r2, int c2)
    {
        r2 = (r2 == -1 ? this->height : r2) - r1;
        c2 = (c2 == -1 ? this->width : c2) - c1;
        Matrix& tmp = *new Matrix(r2, c2);
        for(int r = 0; r < r2; ++r)
            for(int c = 0; c < c2; ++c)
                tmp.content[r][c] = this->content[r1+r][c1+c];
        return tmp;
    }

    // Conatenates this and other matrix. If "side":
    // HORISONTAL: appends other to the left of this matrix
    // VERTICAL: appends other to the bottom of this matrix
    // Returns concatenated matrix.
    Matrix& concatenate(Matrix& other, int side)
    {
        Matrix tmp(other);
        Matrix& res = *new Matrix(*this);
        if(side == VERTICAL) { tmp = tmp.T(); res = res.T(); }

        if(this->height != other.height) throw runtime_error(DIM_ERR);
        res.resize(this->height, this->width + other.width);
        int add = this->width;

        for(int r = 0; r < tmp.height; ++r)
            for(int c = 0; c < tmp.width; ++c)
                res.content[r][add+c] = tmp.content[r][c];

        if(side == VERTICAL) { tmp = tmp.T(); res = res.T(); }
        return res;
    }

    // Fills matrix with given value.
    void fill(V value)
    {
        for(int r = 0; r < height; ++r)
            for(int c = 0; c < width; ++c)
                this->content[r][c] = value;
    }

    // Gets reference to element
    V& operator()(int row, int column) { return content[row][column]; }

    // Returns number of width
    int getWidth() { return width; }
    // Returns number of height
    int getHeight() { return height; }

    // Sets size of this matrix equal to other and copies content.
    // returns reference to this matrix.
    Matrix& operator=(Matrix& other)
    {
        this->resize(other.height, other.width);
        for(int r = 0; r < other.height; ++r)
            for(int c = 0; c < other.width; ++c)
                this->content[r][c] = other.content[r][c];
        return *this;
    }

    // Creates and returns new matrix, in which every element equals
    // sum of corresponding elements of this and other matrix.
    // Thheight error if dimensions aren't equal (sum can't be computed)
    Matrix& operator+(Matrix& other)
    {
        if((this->width != other.width) || (this->height != other.height))
            { throw runtime_error(DIM_ERR); }

        Matrix& result = *new Matrix(this->height, this->width);

        for(int r = 0; r < this->height; ++r)
            for(int c = 0; c < this->width; ++c)
                result.content[r][c] = this->content[r][c] + other.content[r][c];

        return result;
    }

    // Creates and returns new matrix, in which every element equals
    // difference of corresponding elements of this and other matrix.
    // Thheight error if dimensions aren't equal (difference can't be computed)
    Matrix& operator-(Matrix& other)
    {
        if((this->width != other.width) || (this->height != other.height))
            { throw runtime_error(DIM_ERR); }

        Matrix& result = *new Matrix(this->height, this->width);

        for(int r = 0; r < other.height; ++r)
            for(int c = 0; c < other.width; ++c)
                result.content[r][c] = this->content[r][c] - other.content[r][c];

        return result;
    }

    // Creates and returns new matrix which is mathematical product of given matrices.
    // Thheight error if this.height != other.width (product can't be computed)
    Matrix& operator*(Matrix& other)
    {
        if(this->width != other.height) throw runtime_error(DIM_ERR);
        Matrix& result = *new Matrix(this->height, other.width);
        for(int r = 0; r < this->height; ++r)
        {
            for(int c = 0; c < other.width; ++c)
            {
                result.content[r][c] = 0;
                for(int i = 0; i < this->width; ++i)
                {
                    result.content[r][c] += this->content[r][i] * other.content[i][c];
                }
            }
        }
        return result;
    }

    // Creates and returns transposed matrix.
    Matrix& T()
    {
        Matrix& result = *new Matrix(this->width, this->height);

        for(int r = 0; r < this->height; ++r)
            for(int c = 0; c < this->width; ++c)
                result.content[c][r] = this->content[r][c];
        
        return result;
    }

    // function for matrix input
    friend std::istream& operator>>(std::istream& inpStream, const Matrix& matrix)
    {
        for(int r = 0; r < matrix.height; ++r)
            for(int c = 0; c < matrix.width; ++c)
                inpStream >> matrix.content[r][c];
        
        return inpStream;
    }

    // function for matrix output
    friend std::ostream& operator<<(std::ostream& outStream, const Matrix& matrix)
    {
        for(int r = 0; r < matrix.height; ++r)
        {
            if(abs(matrix.content[r][0]) < FLOAT_ERR) outStream << (double)0.0;
            else outStream << matrix.content[r][0];
            for(int c = 1; c < matrix.width; ++c)
                if(abs(matrix.content[r][c]) < FLOAT_ERR) outStream << ' ' << (double)0.0;
                else outStream << ' ' << matrix.content[r][c];
            outStream << '\n';
        }
        return outStream;
    }

protected:
    V **content;
    int width;
    int height;
};

template<typename V>
class SquareMatrix : public Matrix<V>
{
public:
    // Constructor; creates matrix with dimensions size X size
    SquareMatrix(int size) : Matrix<V>(size, size) { }

    // Copy constructor; creates new matrix equal to other.
    SquareMatrix(Matrix<V>& other) : Matrix<V>(other) { }

    // Reloaded = operator. Other matrix should be with equal dimension sizes.
    SquareMatrix& operator=(Matrix<V>& other)
    {
        if(other.getWidth() != other.getHeight()) throw runtime_error(DIM_ERR);
        return *(SquareMatrix<V>*)(&Matrix<V>::operator=(other));
    }

    // Other operators overloading is not necessary. SquareMatrices
    // will be upcasted to Matrix with equal dimension sizes automatically.

    // Resizes this matrix. No operations are performed on content
    // (elements outside resized matrix will be lost and newly added elements
    // can store trash; however, all other elements will retain their values).
    // Returns reference to this matrix.
    SquareMatrix& resize(int size) { Matrix<V>::resize(size, size); return *this; }
    // Resizing with unequal dimension sizes is prohibited for SquareMatrix.
    Matrix<V>& resize(int height, int width) = delete;
};

template<typename V>
class Identity : public SquareMatrix<V>
{
public:
    Identity(int size = 0) : SquareMatrix<V>(size)
    {
        for(int i = 0; i < size; ++i)
            for(int j = 0; j < size; ++j)
                this->content[i][j] = (int)(i == j);
    }
};

template<typename V>
class Permutation : public SquareMatrix<V>
{
public:
    Permutation(int size, int r1, int r2) : SquareMatrix<V>(size)
    {
        for(int r = 0; r < size; ++r)
        {
            for(int c = 0; c < size; ++c)
            {
                if (r == r1) this->content[r][c] = (V)(r2 == c);
                else if (r == r2) this->content[r][c] = (V)(r1 == c);
                else this->content[r][c] = (V)(r == c);
            }
        }
    }
};

template<typename V>
class Elimination : public Identity<V>
{
public:
    Elimination(int size, int r, int c, V element) : Identity<V>(size)
        { this->content[r][c] = element; }
};

template<typename V>
class ColumnVector : public Matrix<V>
{
public:
    // Constructor; creates matrix with dimensions width X height
    ColumnVector(int size) : Matrix<V>(size, 1) { }

    // Copy constructor; creates new matrix equal to other.
    ColumnVector(ColumnVector<V>& other) : Matrix<V>(other) { }

    ColumnVector(Matrix<V>& other) : Matrix<V>(other)
        { if(other.getWidth() != 1) throw runtime_error(DIM_ERR); }
    
    ColumnVector& operator=(Matrix<V>& other)
    {
        if(other.getWidth() != 1) throw runtime_error(DIM_ERR);
        return *(ColumnVector<V>*)(&Matrix<V>::operator=(other));
    }

    ColumnVector& resize(int size) { Matrix<V>::resize(size, 1); return *this; }
    // Resizing as in Matrix is prohibited for ColumnVector.
    Matrix<V>& resize(int height, int width) = delete;
    V& operator()(int row) { return this->content[row][0]; }

    // computes the norm of vector
    V abs()
    {
        V res = 0;
        for(int i=0; i<this->height; ++i)
            res += this->content[i][1] * this->content[i][1];
        return sqrt(res);
    }
};

template<typename V>
int GEForward(Matrix<V>& matrix)
{   
    int detSign = 1;
    int size = matrix.getHeight();
    for(int c = 0; c < size-1; ++c)
    {
        int mxr = c;
        for(int r = c+1; r < size; ++r)
            if(abs(matrix(r, c)) > abs(matrix(mxr, c)))
                mxr = r;

        if(mxr != c)
        {
            matrix = Permutation<V>(size, c, mxr) * matrix;
            detSign *= -1;
        }

        for(int r = c+1; r < size; ++r)
        {
            if(matrix(r, c) == 0) continue;

            matrix = Elimination<V>(size, r, c, -matrix(r, c) / matrix(c, c))
                        * matrix;
        }
    }
    return detSign;
}

template<typename V>
void GEBackward(Matrix<V>& matrix)
{
    int size = matrix.getHeight();
    for(int c = size-1; c > 0; --c)
    {
        for(int r = c-1; r >= 0; --r)
        {
            if(matrix(r, c) == 0) continue;

            matrix = Elimination<V>(size, r, c, -matrix(r, c) / matrix(c, c))
                        * matrix;
        }
    }
}

template<typename V>
void GEFull(Matrix<V>& matrix)
{
    GEForward(matrix);
    GEBackward(matrix);

    for(int r = 0; r < matrix.getHeight(); ++r)
    {
        V tmp = matrix(r, r);
        for(int c = 0; c < matrix.getWidth(); ++c)
            matrix(r, c) /= tmp;
    }
}

template<typename V>
Matrix<V>& getInverse(Matrix<V>& source)
{
    Matrix<V> matrix = source;
    Identity<double> tmp(matrix.getHeight());
    matrix = matrix.concatenate(tmp, HORISONTAL);

    GEFull(matrix);

    return matrix.submatrix(0, source.getWidth(), -1, -1);
}

template<typename V>
ColumnVector<V>& leastSquaresApproximation(ColumnVector<V>& data, ColumnVector<V>& b, int degree)
{
    //cout << data << b;
    Matrix<V>& matrix = *new Matrix<V>(data.getHeight(), degree + 1);
    for(int pw = 0; pw <= degree; ++pw)
        for(int i = 0; i < data.getHeight(); ++i)
            matrix(i, pw) = pow(data(i), pw);

    cout << "A:\n" << matrix;
    
    //Matrix<V> matrixT = matrix.T;
    b = matrix.T() * b;
    matrix = matrix.T() * matrix;

    cout << "A_T*A:\n" << matrix;

    matrix = getInverse(matrix);
    
    cout << "(A_T*A)^-1:\n" << matrix;

    cout << "A_T*b:\n" << b;

    matrix = matrix * b;

    return (ColumnVector<double>&)matrix;
}

template<typename V>
V findY(ColumnVector<V>& k, V x)
{
    V res = 0;
    for(int p = 0; p < k.getHeight(); ++p)
    {
        res += k(p) * pow(x, p);
    }
    return res;
}

int main()
{
    FILE* gnuPipe = _popen(GNUPLOT_CMD, "w");
    std::ifstream input("input.txt", std::ifstream::in);

    int n, degree;
    input >> n >> degree;
    Matrix<double> source(n, 2);
    input >> source;
    
    ColumnVector<double> res = leastSquaresApproximation(
        (ColumnVector<double>&)source.submatrix(0, 0, n, 1),
        (ColumnVector<double>&)source.submatrix(0, 1, n, 2),
        degree
    );
    cout << "x~:\n" << res << '\n';
    
    double xi, xm, yi, ym, nOfSteps = 200;
    for(int r = 0; r < source.getHeight(); ++r)
    {
        xi = min(xi, source(r, 0));
        xm = max(xm, source(r, 0));
        yi = min(yi, source(r, 1));
        ym = max(ym, source(r, 1));
    }
    xi -= (xm - xi) / 3.;
    xm += (xm - xi) / 4.;
    yi -= (ym - yi) / 3.;
    ym += (ym - yi) / 4.;
    
    fprintf(gnuPipe, "set style circle radius graph 0.004\n");
    fprintf(gnuPipe, "set style fill solid 1.0\n");
    fprintf(gnuPipe, "unset autoscale\n");
    fprintf(gnuPipe, "set xrange [%f:%f]\n", xi, xm);
    fprintf(gnuPipe, "set yrange [%f:%f]\n", yi, ym);
    
    fprintf(gnuPipe, "plot '-' title 'initial data' with circles, %f", res(0));
    for(int r = 1; r < res.getHeight(); ++r)
        fprintf(gnuPipe, "+(%f)*(x**%d)", res(r), r);
    fprintf(gnuPipe, " title 'fitted polynomial' with lines\n");

    for(int r = 0; r < source.getHeight(); ++r)
        fprintf(gnuPipe, "%f\t%f\n", source(r, 0), source(r, 1));
    fprintf(gnuPipe, "e\n");

    fflush(gnuPipe);
    _pclose(gnuPipe);
    return 0;
}
