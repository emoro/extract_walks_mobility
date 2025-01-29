#include <Rcpp.h>
using namespace Rcpp;

//Haversine function, assumes spherical earth.
// [[Rcpp::export]]
double haverDist(double lonA, double latA, double lonB, double latB,
        int EARTH_RADIUS_METERS = 6378137) {
    lonA = lonA * M_PI/180;
    latA = latA * M_PI/180;
    lonB = lonB * M_PI/180;
    latB = latB * M_PI/180;

    double v = sin((lonA - lonB)/2);
    double u = sin((latA - latB)/2);
    double dist = 2.0 * EARTH_RADIUS_METERS * asin(sqrt(u * u + cos(latA) * cos(latB) * v * v));
    return dist;
}

// [[Rcpp::export]]
NumericVector haverDistVector(NumericVector lon, NumericVector lat,
        int EARTH_RADIUS_METERS = 6378137) {
    lon = lon * M_PI/180;
    lat = lat * M_PI/180;

    NumericVector ret(lat.size());
    ret[0] = 0;
    int n = ret.size();

    for(int i=1; i < n; i++) {
        double v = sin((lon[i] - lon[i-1])/2);
        double u = sin((lat[i] - lat[i-1])/2);
        ret[i] = 2.0 * EARTH_RADIUS_METERS * asin(sqrt(u * u + cos(lat[i]) * cos(lat[i-1]) * v * v));
    }
    return ret;
}

// [[Rcpp::export]]
double findAng(double lonA, double latA, double lonB, double latB, double lonC,
        double latC) {
    double ab = haverDist(lonA, latA, lonB, latB);
    double bc = haverDist(lonB, latB, lonC, latC);
    double ac = haverDist(lonA, latA, lonC, latC);
    double ret = acos((ab*ab + bc*bc - ac*ac)/(2*ab*bc));
    return ret;
}

// [[Rcpp::export]]
NumericVector findAngVector(NumericVector lon, NumericVector lat, NumericVector dist) {
    NumericVector ret(lat.size());
    int n = ret.size();
    ret[0] = NumericVector::get_na();
    ret[n-1] =  NumericVector::get_na();

    // a is i-1, b is i and c is i+1. It computes angle at b.
    for(int i=1; i < n-1; i++) {
        double ab = dist[i];
        double bc = dist[i+1];
        double ac = haverDist(lon[i-1], lat[i-1], lon[i+1], lat[i+1]);
        ret[i] = acos((ab*ab + bc*bc - ac*ac)/(2*ab*bc));
    }
    return ret;
}

// Find when standing still
// [[Rcpp::export]]
LogicalVector makeLoIsStillHelper(LogicalVector ret, NumericVector accuracy,
        NumericVector lonDeg, NumericVector latDeg, NumericVector userFactor) {

    //vectorized operations
    int n = ret.size();

    int start = 0;
    //Skip i==0 on purpose  
    for(int i=1; i < n; i++) {
        if (userFactor[i] != userFactor[start]) {
            start = i;
            continue;
        }

        double dist = haverDist(latDeg[i], lonDeg[i], latDeg[start], lonDeg[start]);
        if (dist < accuracy[i] + accuracy[start]) {
            ret[i] = true;
        } else {
            start = i;
        }
    }
    return ret;
}

