import matplotlib.pyplot as plt
#%matplotlib inline
import seaborn as sns
import numpy as np
import pandas as pd
from scipy.stats import iqr
import statsmodels.formula.api as smf

def low_variability(df,umbral,target):
    meds=[]
    iqs=[]
    for var in [x for x in list(df) if not (x==target)]:#quitamos de las variables la target
        meds.append(np.percentile(df[var],50))
        iqs.append(iqr(df[var]))
        
        if iqr(df[var])<umbral:
            print('se elimina:',var)
            df.drop(var, axis=1, inplace=True)
            
    plt.scatter(meds,iqs)
    plt.plot([0, 200], [np.percentile(np.array(iqs),10), np.percentile(np.array(iqs),10)], 'r-',label='p10 de IQR',linewidth=4,color='red')
    plt.xlabel("Medianas")
    plt.ylabel("IQR")
    plt.title('Medianas vs. Rangos IC') 
    plt.legend(loc="center")
    plt.show()

    print('corte IQR percentil 10',np.percentile(np.array(iqs),10))
    
    
#Función que regresa la lista de variables a eliminar....
def correlation_filtering(df,target,umbral):
    elimina=[]#lista de campos a eliminar
    dfc=df.corr()
    crf=abs(dfc[dfc<1]).describe()
    sns.boxplot(x=crf.ix['max'],orient='v')
    plt.title('Distribución de correlaciones máximas por variable en valor absoluto')
    plt.show()
    
    for var in [x for x in list(df) if not (x==target)]:
        if abs(dfc[dfc[var]<1][var]).max()>umbral:
            
            corvar=dfc[abs(dfc[var])==abs(dfc[dfc[var]<1][var]).max()].index[0]
            print(var, corvar)
            #Cuál tiramos?... la que este menos correlacionada con el target
            if (abs(np.corrcoef(df[var],df[target])[0][1])<abs(np.corrcoef(df[corvar],df[target])[0][1])):
                el=var
                print('elimina',var)
            else:
                el=corvar
                print('elimina',corvar)
            
            if el not in elimina:
                elimina.append(el)
                
    return(elimina)   



#Función para eliminar variables correlacionadas basado en que estas se relacionan con la variable target
def FCB_filtering(df,umbral,target):#la función regresa la lista de variables a conservar ojo!
    
    kp=[]
    features=[x for x in list(df) if not (x==target)]
    numf=1
    aux=df
    while(numf>0):#mientras tengamos una lista de variables correlacionadas econ el target...
        dfc=aux[features].corr()#hacemos la matriz de correlaciones de las variables distintas del target
    
        tabcor=pd.DataFrame(features,columns=[['variable']])
        tabcor['correlacion']=0#creamos un dataframe con una columna donde llenaremos la correlación de
        #de las variables vs. el target
        j=0
        for i in tabcor['variable']:
            tabcor['correlacion'].iloc[j]=np.corrcoef(aux[i], aux[target])[1][0]#correlación de variables y target
            j=j+1
        tabcor['abscor']=abs(tabcor['correlacion'])#creamos correlaciones en valor absoluto para ordenar
        #las variables más correlacionadas con el target positiva o negativamente
        tabcor.sort_values(by='correlacion',ascending=False) 
        ax = plt.axes()
        sns.barplot(y='variable',x='correlacion',data=tabcor.sort_values(by='correlacion',ascending=False))
        ax.set_title('Correlación variables con el target')
        plt.show()#graficamos las correlaciones vs. el target en cada paso
        sal=tabcor.sort_values(by='abscor',ascending=False)
        sal=sal[abs(sal['abscor'])>umbral]
        numf=len(sal)
        
        if numf>0:
            
            maxvar=sal['variable'].iloc[0]#tomamos la variable con mayor correlación
            kp.append(maxvar)
            ll=dfc[abs(dfc[maxvar])>umbral][maxvar].index
            #ll=ll[ll!=maxvar]
            print(' tira ',ll[ll!=maxvar])
            print(sal)
            aux=aux.drop(ll, axis=1, inplace=False)#borramos las variables más corelacionadas con nuestra
            #variable número1(la más correlacionada con el target)
            features=[x for x in list(aux) if not (x==target)]#quitamos las variables correlacionadas incluyendo
            #la variable de referencia para dar paso a la siguiente de la lista.

    return(kp+list(aux))


#Funcion forward_filtering
def forward_filtering(df,target,varbs):
    ll=[]#declaramos la lista que vamos a llenar con las variables finales a seleccionar
    rfinal=-np.inf#vamos a usar R2 como métrica de éxito del modelo y escogeremos la lista donde se maximice este
    #dato.
    for k in range(len(varbs)):#vamos a recorrer todas las variables y después elegiremos la lista de mejor R2
        score_max=-np.inf#inicializamos este score que será el estadístico F de cada combinación de variables.
        #cross validation vamos a recorrer cada variable con la escogida en el paso anterior.
        for var in varbs:
            newll=ll+[var]#.append(var)#agregamos la variable a la lista
            f = target + ' ~ ' + '+'.join(newll)#hacemos la expresion target~variables para llamar un regresion
            mod = smf.ols(formula = str(f), data = df).fit()#ajustamos la regresion con las variables en ll
            score = mod.fvalue#obtenemos el valor f de cada lista para escoger la mejor...
            if score>score_max:#si encontramos una seleccion que supere la mejor f la cambiamos
                rs,score_max, var_i, rnewll =np.round(mod.rsquared,2),score, var, newll
        
        if rs>rfinal:#si encontramos una lista de variables con mejor r2 la cambiamos
            print(rfinal)
            rfinal=rs
            ll=rnewll
            print(ll)
            print(score_max)
            print(rs)
            varbs.remove(var_i)
            
    modelo_fin=smf.ols(formula = str(target + ' ~ ' + '+'.join(ll)), data = df).fit() #modelo final y variabes
    print (modelo_fin.summary())
    return(ll)#regresa lista de variables a escoger..



