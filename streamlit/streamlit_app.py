import streamlit as st
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Define the Streamlit app
def app():
    # Load the data from GitHub
    url = 'https://raw.githubusercontent.com/outside-edge/toss-up/master/data/final_output.csv'
    df = pd.read_csv(url)

    st.sidebar.title("Visualization Options")
    plot_type = st.sidebar.selectbox("Select plot type", ["histogram", "box plot", "violin plot", "scatter plot", "pair plot"])
    x_var = st.sidebar.selectbox("Select x variable", df.columns)
    y_var = st.sidebar.selectbox("Select y variable", df.columns, index=1)
    facet_col = st.sidebar.selectbox("Select facet column", df.select_dtypes(include='category').columns.tolist(), index=0)
   
    # Create the plot
    fig, ax = plt.subplots()
    if plot_type == "histogram":
        sns.histplot(data=df, x=x_var, hue=facet_col, kde=True, ax=ax)
    elif plot_type == "box plot":
        sns.boxplot(data=df, x=x_var, y=y_var, hue=facet_col, ax=ax)
    elif plot_type == "violin plot":
        sns.violinplot(data=df, x=x_var, y=y_var, hue=facet_col, ax=ax)
    elif plot_type == "scatter plot":
        sns.scatterplot(data=df, x=x_var, y=y_var, hue=facet_col, style=facet_col, ax=ax)
    elif plot_type == "pair plot":
        sns.pairplot(data=df, hue=facet_col)

    # Display the plot
    st.pyplot(fig)

# Run the app
if __name__ == "__main__":
    app()
