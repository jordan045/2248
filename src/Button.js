
function MyButton({onClick,name,color}) {
    return (
        <div
            className="booster"
            style={{backgroundColor: color }}
            onClick={onClick}
        >
            {name}
        </div>
    );
}

export default MyButton;
