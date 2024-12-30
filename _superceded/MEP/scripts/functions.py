'''
Adrian Wiegman, adrian.wiegman@usda.gov
Created: 10:05 AM Thursday, March 30, 2023
Updated:

This script contains helper functions for arcgis workflow
to analyze land use and other datasets within watersheds. 
'''

def fn_hello(x="world"):
    '''
    prints hello x
    '''
    print("hello %s" %x)
    
def fn_try_mkdir(dirname):
    '''
    tries to make a new directory
    uses proper error handling 
    '''
    import os, errno
    try:
        os.mkdir(dirname)
    except OSError as exc:
        if exc.errno != errno.EEXIST:
            raise
        pass

def fn_recursive_glob_search (startDir=None,
                             fileExt="csv"):
    '''returns:
           file paths matching extension 
           within all subdirectories starting directory
       inputs:
           startDir = root or parent directory to start search
           fileExt = file extension, e.g. ".csv" ".xlsx" ".shp"
    '''
    import glob, os
    if startDir is None:
        startDir = os.getcwd
    fileList = []
    glbsearch = os.path.join(startDir,'**/*'+fileExt)
    for f in glob.glob(glbsearch, recursive=True):
        #print(f)
        fileList.append(f)
    return(fileList)

def fn_regex_search_replace(string,pattern,replacement):
    '''
    returns the a string with a pattern substituted by a replacement
    '''
    import re
    x = re.sub(pattern,replacement,string)
    return(x)

def fn_regex_search_0 (string,pattern,noneVal="NA"):
    '''
    returns the first match of a regular expression pattern search on a string
    '''
    import re
    x = re.search(pattern,string)
    if x is None: 
        x= [noneVal]    
    return(x[0])

def fn_sk_model_regression_scores(model,X_test,y_test):
    '''
    returns evaluation metrics from an input of an trained sklearn regressor object and a testing dataset
    '''
    from sklearn.metrics import mean_absolute_error,r2_score,mean_squared_error
    import numpy as np
    y_pred = model.predict(X_test)
    print("R^2 : ", r2_score(y_test, y_pred))
    print("MAE :", mean_absolute_error(y_test,y_pred))
    print("RMSE:",np.sqrt(mean_squared_error(y_test, y_pred)))
    
def fn_sklearn_cross_val_scores(estimator,X,y,cv = None,test_frac=0.3,rnd=0,scoring='r2'):
    '''returns cross validation scores on a dataset given an `estimator`, training features (x), and target features (y)'''
    from sklearn.model_selection import cross_val_score
    from sklearn.model_selection import ShuffleSplit
    import pandas as pd
    import numpy as np
    if cv is None:
        n_samples = X.shape[0]
        n_cv = int(np.floor(n_samples/5))
        print('running cross validation with ShuffleSplit: n_splits=%s, test_size=%s, rand_state = %s' %(n_cv,test_frac,rnd))
        cv = ShuffleSplit(n_splits=n_cv, test_size=test_frac, random_state=rnd)
    scores = cross_val_score(estimator, X, y, cv=cv,scoring='r2')
    print("%0.2f accuracy with a standard deviation of %0.2f" % (scores.mean(), scores.std()))
    return scores

def fn_ensemble_feature_importance_plot(ensmb,feature_names,y_name):
    """
    CALCULATE THE IMPORTANCE FEATURES FROM AN ENSEMBLE LEARNER AND GENERATE BAR PLOTS
    """
    import pandas as pd
    import time
    import numpy as np
    import matplotlib.pyplot as plt
    import seaborn as sns

    start_time = time.time()
    importances_mean = ensmb.feature_importances_
    indexes = []
    importances = []
    for tree in ensmb.estimators_:
        #print(tree.feature_importances_.tolist())
        importances.append(tree.feature_importances_.tolist())
        indexes.append(feature_names.tolist())
    std = np.std(importances_mean, axis=0)
    elapsed_time = time.time() - start_time
    print(f"Elapsed time to compute the importances: {elapsed_time:.3f} seconds")
    #print(indexes)
    index = sum(indexes,[]) # merging list of lists into one list. 
    importance = sum(importances,[])
    df_importance = pd.DataFrame({"varname":index,"importance":importance})
    df_ = df_importance.sort_values(ascending=False,by='importance')
    _ = df_importance.groupby('varname').aggregate(np.mean).rename(columns={'importance':"mean"}).sort_values(ascending=False,by='mean').reset_index()
    #print(_)
    df_ = df_.merge(_[:20],how='inner').sort_values(ascending=False,by='mean')
    fig, ax = plt.subplots()
    sns.barplot(x='importance',y='varname',
                data=df_,
                color='grey',
                orient="h")
    ax.set_title("Feature importances for %s" %y_name)
    ax.set_ylabel("Mean decrease in impurity")
    fig.tight_layout()
    return _
    
def fn_plot_obs_vs_pred(regr,X_test,X_train,y_test,y_train,lab='data'):
    from sklearn.metrics import r2_score
    import seaborn as sns
    import matplotlib.pyplot as plt
    y = y_test.append(y_train)
    pred_train = regr.predict(X_train).flatten()
    pred_test = regr.predict(X_test).flatten()
    r2_train = r2_score(y_train,pred_train)
    r2_test = r2_score(y_test,pred_test)
    sns.scatterplot(x=y_train,y=pred_train,label="train %s r2 = %f" % (lab, r2_train))
    sns.scatterplot(x=y_test,y=pred_test,label="test %s r2 = %f" %(lab, r2_test))
    plt.plot([y.min(),y.max()], [y.min(),y.max()], color='black')

def fn_get_vif(exogs, data):
    import pandas as pd
    import statsmodels.formula.api as smf

    '''Return VIF (variance inflation factor) DataFrame

    Args:
    exogs (list): list of exogenous/independent variables
    data (DataFrame): the df storing all variables

    Returns:
    VIF and Tolerance DataFrame for each exogenous variable

    Notes:
    Assume we have a list of exogenous variable [X1, X2, X3, X4].
    To calculate the VIF and Tolerance for each variable, we regress
    each of them against other exogenous variables. For instance, the
    regression model for X3 is defined as:
                        X3 ~ X1 + X2 + X4
    And then we extract the R-squared from the model to calculate:
                    VIF = 1 / (1 - R-squared)
                    Tolerance = 1 - R-squared
    The cutoff to detect multicollinearity:
                    VIF > 10 or Tolerance < 0.1
    '''

    # initialize dictionaries
    vif_dict, tolerance_dict = {}, {}

    # create formula for each exogenous variable
    for exog in exogs:
        not_exog = [i for i in exogs if i != exog]
        formula = f"{exog} ~ {' + '.join(not_exog)}"

        # extract r-squared from the fit
        r_squared = smf.ols(formula, data=data).fit().rsquared

        # calculate VIF
        vif = 1/(1 - r_squared)
        vif_dict[exog] = vif

        # calculate tolerance
        tolerance = 1 - r_squared
        tolerance_dict[exog] = tolerance

    # return VIF DataFrame
    df_vif = pd.DataFrame({'VIF': vif_dict, 'Tolerance': tolerance_dict})
    print("VIF is not of concern if less than 3")
    return df_vif

def fn_shap_model_explainer(
    m,# fitted model 
    X,# array of explanatory variables
    fig_prefix="rfr",
    # save figures to png
    save_figs = True
    ):
    #%matplotlib auto
    '''
    # use this line to install shap if needed
    !pip install shap
    # https://shap.readthedocs.io/en/latest/example_notebooks/api_examples/plots/heatmap.html
    '''
    import shap
    # Fits the explainer
    explainer = shap.Explainer(m, X) # Calculates the SHAP values - It takes some time
    shap_values = explainer(X)
    
    # Plot beeswarm
    shap.plots.beeswarm(shap_values,max_display=10)
    plt.savefig('{}_shap_beeswarm.png'.format(fig_prefix), format='png', dpi=600, bbox_inches='tight')
    plt.close()
    
    # Plot heatmap
    shap.plots.heatmap(shap_values,instance_order=shap_values.sum(1),max_display=10,show=False)
    plt.savefig('{}_shap_heatmap.png'.format(fig_prefix), format='png', dpi=600, bbox_inches='tight')
    plt.close()
    
    # Plot waterfall
    shap.plots.waterfall(shap_values[0])
    plt.savefig('{}_shap_waterfall_0.png'.format(fig_prefix), format='png', dpi=600, bbox_inches='tight')
    plt.close()
