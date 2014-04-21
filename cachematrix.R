# This function returns a list composed of a matrix of NAs called Inverse and functions that 
# get:() returns the content of the matrix x submitted as argument of makeCacheMatrix
# (set() of the original piece of code was not included because it was useless)
# set_inverse() assigns the inverse of the matrix to the Inverse matrix using the special operator <<-
# because Inverse is outside the environemnt of the function
# get_inverse() returns the matrix Inverse

makeCacheMatrix <- function(x) {
	Inverse <- matrix (nrow = nrow(x), ncol = ncol(x))
	get <- function() x
	set_inverse <- function(Invs) {
		Inverse <<- Invs
		}
	get_inverse <- function() {
		Inverse
	}
	list(get = get, set_inverse = set_inverse,
		get_inverse = get_inverse)
}

# This function returns the inverse of a matrix which was previously submitted as argument to the makeCacheMatrix function.
# The argument for this function is a list returned by makeCacheMatrix().
# The function first retrieves the matrix called Inverse from the list returned with makeCacheMatrix() using the function get_inverse of that list
# It then checks if the first element is NA and if not prints 'Getting inverse of matrix' and returns the matrix (and the following code is not executed)
# In case the first element of the matrix is actually an NA (meaning that the inverse of the matrix has not yet been computed), the function retrieves 
# the content of the matrix originally submitted to makeCacheMatrix() and assigns it to  object M. It then assigns the inverse of M to Inverse.
# It then assigns the object Inverse (in the scope of the function) to the Inverse object of the list submitted as argument, and finally returns the Inverse object.
cacheSolve <- function(x) {
	Inverse <- x$get_inverse()
	if(!is.na(Inverse[1,1])) {
		print("Getting inverse of matrix")
		return(Inverse)
	}
	M <- x$get()
	Inverse <- solve(M)
	x$set_inverse(Inverse)
	Inverse
}